{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handlers that ask the client questions part-way through, written as
-- straight-line code.
--
-- A multi round-trip request ends each round that needs input: the client
-- answers and retries the request, possibly on another server instance.
-- So an t'Ask' handler does not pause at a question. It runs from the start
-- on every round, against a /journal/ of what earlier rounds produced, which
-- the library keeps sealed in the request state:
--
-- * 'elicit' returns the answer recorded for it, or ends the round with the
--   question when there is none yet.
-- * 'step' runs its action once, records the result, and returns the
--   recorded result on later rounds.
--
-- > confirmDelete :: ClientContext -> Text -> Ask ToolResult
-- > confirmDelete _ name = do
-- >   size <- step (fileSize name)
-- >   answer <- elicit ("Delete " <> name <> " (" <> showSize size <> ")?")
-- >   case answer of
-- >     Accepted (Confirm True) -> do
-- >       step (deleteFile name)
-- >       pure (toolResult [ContentText "Deleted"])
-- >     Unavailable -> pure (toolError "Confirmation needed: this client cannot ask")
-- >     _ -> pure (toolResult [ContentText "Kept"])
--
-- Three rules follow from replay:
--
-- * __Ask the same questions in the same order.__ Each 'elicit' and 'step'
--   takes the next position in the journal. If a round asks something
--   different at a position, the request fails with an internal error that
--   names the position, rather than passing an answer to the wrong question.
-- * __'step' is at-least-once.__ If a round's reply is lost and the client
--   retries with an earlier state, the step runs again. Effects that must
--   happen once must be idempotent or guarded on the server.
-- * __'step' results travel to the client and back__ (encrypted) on every
--   later round. Keep them small: an id rather than the object.
--
-- Plain 'liftIO' is not journaled: it runs on every round. Use it for reads
-- that may change between rounds without changing which questions are
-- asked.
--
-- Derived handlers ("MCP.Server.Derive") may return t'Ask' instead of
-- 'IO', and 'MCP.Server.Derive.deriveForm' derives t'Form' instances from
-- records.
module MCP.Server.Ask
  ( -- * Handlers that ask
    Ask
  , runAsk
  , askContext
    -- * Questions
  , Answer(..)
  , Form(..)
  , elicit
  , elicitUrl
    -- * Journaled IO
  , step
    -- * Derived handlers
  , HandlerMonad(..)
  ) where

import           Control.Monad            (ap, liftM)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.Aeson
import qualified Data.Aeson.KeyMap        as KM
import           Data.Aeson.Types         (parseEither)
import qualified Data.ByteString.Lazy     as BSL
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE

import           MCP.Server.Internal.Json (jsonDigest)
import           MCP.Server.Types

-- | How the user answered a question.
data Answer a
  = Accepted a
    -- ^ The user submitted the form (for 'elicitUrl': completed the
    --   interaction).
  | Declined
    -- ^ The user explicitly declined.
  | Cancelled
    -- ^ The user dismissed the question without choosing.
  | Unavailable
    -- ^ The client cannot ask: it speaks a protocol revision without
    --   multi round-trip requests, or did not declare the elicitation
    --   mode. No round trip took place.
  deriving (Show, Eq, Functor)

-- | A form to elicit: an object of strings, numbers, booleans and
-- enumerations, as elicitation allows, and how to read what the user
-- submitted. 'formParse' defaults to the type's 'FromJSON' instance.
--
-- > data Confirm = Confirm { proceed :: Bool } deriving Generic
-- > instance FromJSON Confirm
-- > instance Form Confirm where
-- >   formSchema _ = schema $ SchemaObject [("proceed", schema SchemaBoolean)] ["proceed"]
class Form a where
  -- | The requested schema.
  formSchema :: Proxy a -> Schema
  -- | Read the submitted content, or say why it does not fit the form.
  formParse :: Map Text Value -> Either Text a
  default formParse :: FromJSON a => Map Text Value -> Either Text a
  formParse content = either (Left . T.pack) Right (parseEither parseJSON (toJSON content))

-- | A handler that can ask the client for input. Run it with 'runAsk'.
newtype Ask a = Ask (ClientContext -> Run -> IO (Round a))

-- Where a run is in the journal.
data Run = Run
  { runReplay   :: [Entry]     -- entries from earlier rounds not yet reached
  , runPending  :: Maybe Text  -- fingerprint of the question the last round asked
  , runPosition :: Int
  , runWritten  :: [Entry]     -- the journal so far, newest first
  }

data Round a
  = Done a Run
  | Suspended Int Text InputRequest [Entry]  -- position, fingerprint, request, journal
  | Failed Error

instance Functor Ask where
  fmap = liftM

instance Applicative Ask where
  pure a = Ask $ \_ run -> pure (Done a run)
  (<*>) = ap

instance Monad Ask where
  Ask m >>= k = Ask $ \ctx run -> do
    result <- m ctx run
    case result of
      Done a run'                -> let Ask m' = k a in m' ctx run'
      Suspended pos fp req entries -> pure (Suspended pos fp req entries)
      Failed err                 -> pure (Failed err)

-- | Runs on every round; not journaled.
instance MonadIO Ask where
  liftIO io = Ask $ \_ run -> (\a -> Done a run) <$> io

-- | The context of the current round's request.
askContext :: Ask ClientContext
askContext = Ask $ \ctx run -> pure (Done ctx run)

-- | Run a handler for one round: 'Complete' when it finishes, or
-- 'InputRequired' with its next question and its journal as the state.
-- Use it in a low-level call handler; derived handlers are run with it
-- for you (see t'HandlerMonad').
runAsk :: Ask a -> ClientContext -> IO (Either Error (Outcome a))
runAsk (Ask m) ctx = case readJournal (clientRequestState ctx) of
  Left err -> pure (Left (InternalError err))
  Right (Journal entries pending) -> do
    result <- m ctx Run
      { runReplay = entries
      , runPending = pending
      , runPosition = 0
      , runWritten = []
      }
    pure $ case result of
      Done a run
        | null (runReplay run), Nothing <- runPending run -> Right (Complete a)
        | otherwise -> Left $ mismatch (runPosition run)
            "the handler finished before reaching it"
      Suspended pos fp request written -> Right $ InputRequired
        (Map.singleton (positionKey pos) request)
        (Just (writeJournal (Journal written (Just fp))))
      Failed err -> Left err

-- | The monads a derived handler can run in: 'IO' for handlers that never
-- ask, and t'Ask' for handlers that do. The @derive*Handler@ splices run
-- the handler through this class, so either return type works and a handler
-- switches by changing its type alone.
class Monad m => HandlerMonad m where
  -- | Run a handler for one round of a request.
  runHandler :: ClientContext -> m a -> IO (Either Error (Outcome a))

instance HandlerMonad IO where
  runHandler _ action = Right . Complete <$> action

instance HandlerMonad Ask where
  runHandler ctx handler = runAsk handler ctx

-- | Ask the user to fill in the form for @a@, with a message.
elicit :: Form a => Text -> Ask (Answer a)
elicit message = result
  where
    result = question (ElicitForm message (formSchema (proxyOf result))) parse
    parse (Just content) = formParse content
    parse Nothing        = Left "accepted without content"
    proxyOf :: Ask (Answer a) -> Proxy a
    proxyOf _ = Proxy

-- | Send the user to a URL to complete an interaction out of band, with a
-- message. 'Accepted' means the user agreed to go; the interaction's
-- result reaches the server by other means.
elicitUrl :: Text -> URI -> Ask (Answer ())
elicitUrl message url = question (ElicitUrl message url) (const (Right ()))

-- | Run an action once, and replay its result on later rounds. The result
-- is stored in the request state, so it must survive a JSON round trip; it
-- is returned decoded on every round, the first included.
step :: (ToJSON a, FromJSON a) => IO a -> Ask a
step action = Ask $ \_ run -> case (runReplay run, runPending run) of
  (Stepped v : rest, _) -> pure $ case fromJSON v of
    Success a -> Done a (advance (Stepped v) run { runReplay = rest })
    Error err -> Failed $ InternalError $
      "the step result at position " <> showT (runPosition run)
        <> " no longer decodes: " <> T.pack err
  (_ : _, _) -> pure $ Failed $ mismatch (runPosition run) "the handler ran a step instead"
  ([], Just _) -> pure $ Failed $ mismatch (runPosition run) "the handler ran a step instead"
  ([], Nothing) -> do
    v <- toJSON <$> action
    pure $ case fromJSON v of
      Success a -> Done a (advance (Stepped v) run)
      Error err -> Failed $ InternalError $
        "the step result at position " <> showT (runPosition run)
          <> " does not decode from its own JSON: " <> T.pack err

-- A question at the next position: the request, and how to read an
-- accepted answer's content.
question :: InputRequest -> (Maybe (Map Text Value) -> Either Text a) -> Ask (Answer a)
question request parse = Ask $ \ctx run ->
  let pos = runPosition run
      key = positionKey pos
      fp = jsonDigest (toJSON request)
      record recorded = advance (Asked fp recorded) run
      suspend = Suspended pos fp request (reverse (runWritten run))
      supported = clientSupportsInput ctx request
  in pure $ case (runReplay run, runPending run) of
    (Asked fp' recorded : rest, _)
      | fp' == fp -> case answerOf parse recorded of
          Right a  -> Done a (record recorded) { runReplay = rest }
          Left err -> Failed $ InternalError $
            "the answer at position " <> key <> " no longer parses: " <> err
    (_ : _, _) -> Failed $ mismatch pos "the handler asked a different question"
    ([], Just fp')
      | fp' /= fp -> Failed $ mismatch pos "the handler asked a different question"
      | otherwise -> case Map.lookup key (clientInputResponses ctx) of
          Nothing
            | supported -> suspend
            | otherwise -> Done Unavailable (record Unavailable') { runPending = Nothing }
          Just response -> case recordResponse response >>= \r -> (,) r <$> answerOf parse r of
            Right (recorded, a) -> Done a (record recorded) { runPending = Nothing }
            Left err -> Failed $ InvalidParams $ "inputResponses: " <> key <> ": " <> err
    ([], Nothing)
      | supported -> suspend
      | otherwise -> Done Unavailable (record Unavailable')

advance :: Entry -> Run -> Run
advance entry run = run
  { runPosition = runPosition run + 1
  , runWritten = entry : runWritten run
  }

mismatch :: Int -> Text -> Error
mismatch pos what = InternalError $
  "the handler does not match its journal at position " <> showT pos <> ": "
    <> what <> ". Ask handlers must ask the same questions and run the same steps"
    <> " in the same order on every round"

positionKey :: Int -> Text
positionKey = showT

showT :: Show a => a -> Text
showT = T.pack . show

-- The journal ------------------------------------------------------------

data Journal = Journal [Entry] (Maybe Text)

data Entry
  = Asked Text Recorded  -- the question's fingerprint, and its answer
  | Stepped Value

data Recorded
  = Accepted' (Maybe (Map Text Value))
  | Declined'
  | Cancelled'
  | Unavailable'

recordResponse :: InputResponse -> Either Text Recorded
recordResponse (ElicitResponse action content) = Right $ case action of
  ElicitAccept  -> Accepted' content
  ElicitDecline -> Declined'
  ElicitCancel  -> Cancelled'
recordResponse (RawInputResponse _) = Left "expected an elicitation result"

answerOf :: (Maybe (Map Text Value) -> Either Text a) -> Recorded -> Either Text (Answer a)
answerOf parse (Accepted' content) = Accepted <$> parse content
answerOf _ Declined'               = Right Declined
answerOf _ Cancelled'              = Right Cancelled
answerOf _ Unavailable'            = Right Unavailable

instance ToJSON Entry where
  toJSON (Asked fp recorded) = object $ ("q" .= fp) : case recorded of
    Accepted' content -> ["a" .= ("accept" :: Text), "c" .= content]
    Declined'         -> ["a" .= ("decline" :: Text)]
    Cancelled'        -> ["a" .= ("cancel" :: Text)]
    Unavailable'      -> ["a" .= ("unavailable" :: Text)]
  toJSON (Stepped v) = object ["s" .= v]

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \o -> do
    -- By presence: a step's result may itself be null.
    case KM.lookup "s" o of
      Just v -> pure (Stepped v)
      Nothing -> do
        fp <- o .: "q"
        action <- o .: "a"
        Asked fp <$> case action :: Text of
          "accept"      -> Accepted' <$> o .:? "c"
          "decline"     -> pure Declined'
          "cancel"      -> pure Cancelled'
          "unavailable" -> pure Unavailable'
          _             -> fail "unknown answer"

writeJournal :: Journal -> Text
writeJournal (Journal entries pending) = TE.decodeUtf8 $ BSL.toStrict $ encode $ object
  [ "v" .= (1 :: Int)
  , "j" .= entries
  , "p" .= pending
  ]

readJournal :: Maybe Text -> Either Text Journal
readJournal Nothing = Right (Journal [] Nothing)
readJournal (Just state) =
  either (\err -> Left ("the request state is not an Ask journal: " <> T.pack err)) Right $
    eitherDecodeStrict (TE.encodeUtf8 state) >>= parseEither parser
  where
    parser = withObject "Journal" $ \o -> do
      version <- o .: "v"
      if version /= (1 :: Int)
        then fail "unsupported journal version"
        else Journal <$> o .: "j" <*> o .:? "p"
