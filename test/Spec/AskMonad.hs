{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Coverage for the 'Ask' monad (ADR 0009, phase 2): handlers replayed
-- against a journal across rounds, through the real dispatcher and state
-- codec.
module Spec.AskMonad (spec) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import qualified Data.Aeson.KeyMap      as KM
import           Data.Aeson.Types       (Pair)
import           Data.IORef
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           MCP.Server
import           MCP.Server.Handlers    (handleMcpMessage)
import           MCP.Server.JsonRpc
import           Test.Hspec
import           TestHelpers            (testStateCodec)

newtype Target = Target { environment :: Text } deriving (Show, Eq, Generic)
instance FromJSON Target
instance Form Target where
  formSchema _ = schema $ SchemaObject
    [("environment", schema (SchemaString (Just ["staging", "production"])))]
    ["environment"]

newtype Confirm = Confirm { ok :: Bool } deriving (Show, Eq, Generic)
instance FromJSON Confirm
instance Form Confirm where
  formSchema _ = schema $ SchemaObject [("ok", schema SchemaBoolean)] ["ok"]

-- | Counters for what ran: the journaled planning step, and every round.
data Counters = Counters { planRuns :: IORef Int, rounds :: IORef Int }

-- | Ask for a target, plan once, ask for confirmation.
deploy :: Counters -> Ask Text
deploy counters = do
  liftIO (modifyIORef (rounds counters) (+ 1))
  target <- elicit "Where to?"
  case target of
    Accepted (Target env) -> do
      plan <- step $ do
        modifyIORef (planRuns counters) (+ 1)
        pure ("plan-" <> env)
      confirmed <- elicit ("Apply " <> plan <> "?")
      pure $ case confirmed of
        Accepted (Confirm True) -> "applied " <> plan
        other                   -> "not applied: " <> T.pack (show other)
    other -> pure ("no target: " <> T.pack (show other))

-- | Asks a question that changes every round, which replay must refuse.
fickle :: IORef Int -> Ask Text
fickle counter = do
  n <- liftIO (atomicModifyIORef counter (\c -> (c + 1, c)))
  reply <- elicit ("Question " <> T.pack (show n))
  pure (T.pack (show (reply :: Answer Confirm)))

handlersFor :: Counters -> IORef Int -> McpServerHandlers
handlersFor counters fickleCounter = noHandlers
  { tools = Just
      ( \_ -> pure []
      , \ctx name _ -> fmap (fmap (\t -> toolResult [ContentText t])) <$>
          runAsk (if name == "fickle" then fickle fickleCounter else deploy counters) ctx
      )
  }

newCounters :: IO (Counters, IORef Int)
newCounters = (,) <$> (Counters <$> newIORef 0 <*> newIORef 0) <*> newIORef 0

call :: (Counters, IORef Int) -> Text -> Value -> [Pair] -> IO Value
call (counters, fickleCounter) name caps extra = do
  response <- handleMcpMessage (McpServerInfo "T" "1" "") defaultCacheHints testStateCodec
    noNotificationSupport (\_ -> pure ()) (handlersFor counters fickleCounter) anonymousContext $
      JsonRpcMessageRequest JsonRpcRequest
        { requestJsonrpc = "2.0"
        , requestId = RequestIdNumber 1
        , requestMethod = "tools/call"
        , requestParams = Just $ object $ extra ++
            [ "name" .= name
            , "arguments" .= object []
            , "_meta" .= object
                [ "io.modelcontextprotocol/protocolVersion" .= ("2026-07-28" :: Text)
                , "io.modelcontextprotocol/clientCapabilities" .= caps
                ]
            ]
        }
  case response of
    Just (JsonRpcMessageResponse r) -> pure (toJSON r)
    other -> fail ("expected a response, got " ++ show other)

formCaps, noCaps :: Value
formCaps = object ["elicitation" .= object ["form" .= object []]]
noCaps = object []

at :: [Key] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup k o >>= at ks
at _ _ = Nothing

stateOf :: Value -> Text
stateOf v = case at ["result", "requestState"] v of
  Just (String s) -> s
  _               -> error ("no requestState in " ++ show v)

-- | The keys of an @input_required@ result's requests, and their messages.
questionsOf :: Value -> [(Key, Value)]
questionsOf v = case at ["result", "inputRequests"] v of
  Just (Object rs) -> [(k, m) | (k, r) <- KM.toList rs, Just m <- [at ["params", "message"] r]]
  _                -> []

textOf :: Value -> Maybe Value
textOf v = case at ["result", "content"] v of
  Just (Array xs) | [c] <- foldr (:) [] xs -> at ["text"] c
  _                                       -> Nothing

errorOf :: Value -> Maybe (Int, Text)
errorOf v = do
  Number code <- at ["error", "code"] v
  String msg <- at ["error", "message"] v
  pure (round code, msg)

answer :: Key -> Value -> Pair
answer key response = "inputResponses" .= object [key .= response]

accepted :: [Pair] -> Value
accepted content = object ["action" .= ("accept" :: Text), "content" .= object content]

retrying :: Value -> [Pair]
retrying previous = ["requestState" .= stateOf previous]

spec :: Spec
spec = describe "The Ask monad" $ do
  it "asks, replays answers and steps, and completes" $ do
    env <- newCounters
    r1 <- call env "deploy" formCaps []
    questionsOf r1 `shouldBe` [("0", String "Where to?")]
    r2 <- call env "deploy" formCaps $
      answer "0" (accepted ["environment" .= ("staging" :: Text)]) : retrying r1
    questionsOf r2 `shouldBe` [("2", String "Apply plan-staging?")]
    r3 <- call env "deploy" formCaps $
      answer "2" (accepted ["ok" .= True]) : retrying r2
    textOf r3 `shouldBe` Just (String "applied plan-staging")
    readIORef (planRuns (fst env)) `shouldReturn` 1
    readIORef (rounds (fst env)) `shouldReturn` 3

  it "keeps earlier answers in the journal, not in the retry" $ do
    env <- newCounters
    r1 <- call env "deploy" formCaps []
    r2 <- call env "deploy" formCaps $
      answer "0" (accepted ["environment" .= ("production" :: Text)]) : retrying r1
    -- The last round's answers only: the first answer comes from the state.
    r3 <- call env "deploy" formCaps $ answer "2" (accepted ["ok" .= False]) : retrying r2
    textOf r3 `shouldBe` Just (String "not applied: Accepted (Confirm {ok = False})")

  it "asks again when the retry carries no answer" $ do
    env <- newCounters
    r1 <- call env "deploy" formCaps []
    r2 <- call env "deploy" formCaps (retrying r1)
    questionsOf r2 `shouldBe` [("0", String "Where to?")]

  it "returns a declined or cancelled answer to the handler" $ do
    env <- newCounters
    r1 <- call env "deploy" formCaps []
    declined <- call env "deploy" formCaps $
      answer "0" (object ["action" .= ("decline" :: Text)]) : retrying r1
    textOf declined `shouldBe` Just (String "no target: Declined")
    cancelled <- call env "deploy" formCaps $
      answer "0" (object ["action" .= ("cancel" :: Text)]) : retrying r1
    textOf cancelled `shouldBe` Just (String "no target: Cancelled")

  it "answers Unavailable without a round trip when the client cannot ask" $ do
    env <- newCounters
    r <- call env "deploy" noCaps []
    textOf r `shouldBe` Just (String "no target: Unavailable")
    legacy <- runAsk (deploy (fst env)) anonymousContext
    legacy `shouldBe` Right (Complete "no target: Unavailable")

  it "runs a step again when a round is retried from an earlier state" $ do
    env <- newCounters
    r1 <- call env "deploy" formCaps []
    let staging = answer "0" (accepted ["environment" .= ("staging" :: Text)]) : retrying r1
    _ <- call env "deploy" formCaps staging
    _ <- call env "deploy" formCaps staging
    readIORef (planRuns (fst env)) `shouldReturn` 2

  it "refuses a handler whose questions change between rounds" $ do
    env <- newCounters
    r1 <- call env "fickle" formCaps []
    questionsOf r1 `shouldBe` [("0", String "Question 0")]
    r2 <- call env "fickle" formCaps $ answer "0" (accepted ["ok" .= True]) : retrying r1
    fmap fst (errorOf r2) `shouldBe` Just (-32603)
    fmap (T.isInfixOf "position 0" . snd) (errorOf r2) `shouldBe` Just True

  it "rejects content that does not fit the form" $ do
    env <- newCounters
    r1 <- call env "deploy" formCaps []
    r2 <- call env "deploy" formCaps $
      answer "0" (accepted ["environment" .= (3 :: Int)]) : retrying r1
    fmap fst (errorOf r2) `shouldBe` Just (-32602)

  it "rejects an answer that is not an elicitation result" $ do
    env <- newCounters
    r1 <- call env "deploy" formCaps []
    r2 <- call env "deploy" formCaps $
      answer "0" (object ["roots" .= ([] :: [Value])]) : retrying r1
    fmap fst (errorOf r2) `shouldBe` Just (-32602)

  it "completes without state when nothing is asked" $ do
    result <- runAsk (step (pure (42 :: Int))) anonymousContext
    result `shouldBe` Right (Complete 42)

  it "fails a run whose journal belongs to a different handler" $ do
    env <- newCounters
    Right (InputRequired _ journal) <- runAsk (deploy (fst env)) anonymousContext
      { clientProtocolVersion = Just "2026-07-28"
      , clientCapabilities = Just formCaps
      }
    result <- runAsk (step (pure (1 :: Int))) anonymousContext { clientRequestState = journal }
    case result of
      Left (InternalError msg) -> msg `shouldSatisfy` T.isInfixOf "position 0"
      other -> expectationFailure ("expected a mismatch, got " ++ show other)

  it "keeps the pending question's key stable across unrelated rounds" $ do
    env <- newCounters
    r1 <- call env "deploy" formCaps []
    let unrelated = answer "1" (accepted ["environment" .= ("staging" :: Text)]) : retrying r1
    r2 <- call env "deploy" formCaps unrelated
    questionsOf r2 `shouldBe` [("0", String "Where to?")]
