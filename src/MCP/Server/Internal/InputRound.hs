{-# LANGUAGE OverloadedStrings #-}

-- | The library's side of multi round-trip requests: reading a retry's
-- @inputResponses@ and @requestState@ into the handler's context, and
-- turning a handler's 'InputRequired' into an @input_required@ result.
--
-- The sealed state carries, besides the handler's own state, what the spec
-- asks servers to bind it to: the caller (a digest of the principal), the
-- originating request (the method and a digest of its parameters) and an
-- expiry. All four are checked before the handler runs again.
module MCP.Server.Internal.InputRound
  ( beginRound
  , inputRequiredResult
  ) where

import           Control.Monad            (unless, when)
import qualified Crypto.Hash              as Hash
import           Data.Aeson
import qualified Data.Aeson.Key           as Key
import qualified Data.Aeson.KeyMap        as KM
import           Data.Aeson.Types         (parseEither)
import qualified Data.ByteArray           as BA
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy     as BSL
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           System.IO                (hPutStrLn, stderr)

import           MCP.Server.Internal.Json (canonicalJson)
import           MCP.Server.JsonRpc       (JsonRpcError (..))
import           MCP.Server.RequestState
import           MCP.Server.Types

-- | What is sealed into a request state.
data Payload = Payload
  { payloadMethod    :: Text
  , payloadParams    :: Text        -- ^ Digest of the request parameters
  , payloadPrincipal :: Text        -- ^ Digest of the caller's principal
  , payloadExpires   :: Integer     -- ^ POSIX seconds
  , payloadKeys      :: [Text]      -- ^ The keys of the requests issued
  , payloadState     :: Maybe Text  -- ^ The handler's own state
  }

instance ToJSON Payload where
  toJSON p = object
    [ "v" .= (1 :: Int)
    , "m" .= payloadMethod p
    , "d" .= payloadParams p
    , "p" .= payloadPrincipal p
    , "e" .= payloadExpires p
    , "k" .= payloadKeys p
    , "s" .= payloadState p
    ]

instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \o -> do
    version <- o .: "v"
    unless (version == (1 :: Int)) $ fail "unsupported state version"
    Payload <$> o .: "m" <*> o .: "d" <*> o .: "p" <*> o .: "e" <*> o .: "k" <*> o .:? "s"

-- | Prepare the context for a call that may be a retry. Legacy requests
-- have no multi round-trip semantics, so their context is left alone.
--
-- Answers are only taken from a retry that carries the state of the round
-- that asked, and only for the keys that round issued: anything else in
-- @inputResponses@ is ignored, as the spec asks, so a client cannot
-- volunteer answers the handler never requested. A retry's state is
-- verified before anything else; a failure is a @-32602@ the handler never
-- sees.
beginRound :: StateCodec -> Text -> Maybe Value -> ClientContext
           -> IO (Either JsonRpcError ClientContext)
beginRound codec method params ctx
  | Nothing <- clientProtocolVersion ctx = pure (Right ctx)
  | otherwise = case field "requestState" of
      Nothing -> pure (Right ctx)
      Just (String sealed) -> do
        opened <- openPayload codec method params ctx sealed
        pure $ do
          (keys, state) <- opened
          responses <- issuedResponses keys
          Right ctx { clientInputResponses = responses, clientRequestState = state }
      Just _ -> pure $ Left $ invalidParams "Invalid requestState: expected a string"
  where
    field key = case params of
      Just (Object o) -> KM.lookup key o
      _               -> Nothing
    issuedResponses keys = case field "inputResponses" of
      Nothing          -> Right Map.empty
      Just (Object rs) -> Map.fromList <$> traverse parseIssued
        [ (key, v) | key <- keys, Just v <- [KM.lookup (Key.fromText key) rs] ]
      Just _           -> Left $ invalidParams "Invalid inputResponses: expected an object"
    parseIssued (key, v) = case parseEither parseJSON v of
      Right response -> Right (key, response)
      Left err -> Left $ invalidParams $ "Invalid inputResponses: " <> key <> ": " <> T.pack err

-- | The @input_required@ result for a handler's requests and state, or a
-- @-32603@ when the handler asked for input this client cannot provide
-- (a server bug: handlers should check 'clientSupportsInput').
inputRequiredResult :: StateCodec -> Text -> Maybe Value -> ClientContext
                    -> Map Text InputRequest -> Maybe Text
                    -> IO (Either JsonRpcError Value)
inputRequiredResult codec method params ctx requests state
  | Nothing <- clientProtocolVersion ctx =
      pure $ Left $ internalError
        "the handler requested input, which the client's protocol revision does not support"
  | unsupported@(_ : _) <- [key | (key, request) <- Map.toList requests
                                , not (clientSupportsInput ctx request)] =
      pure $ Left $ internalError $
        "the handler requested input the client cannot provide ("
          <> T.intercalate ", " unsupported <> ")"
  | otherwise = do
      now <- getPOSIXTime
      let payload = Payload
            { payloadMethod = method
            , payloadParams = paramsDigest params
            , payloadPrincipal = principalDigest ctx
            , payloadExpires = floor (now + stateLifetime codec)
            , payloadKeys = Map.keys requests
            , payloadState = state
            }
      sealed <- sealState codec (BSL.toStrict (encode payload))
      pure $ Right $ object
        [ "resultType" .= ("input_required" :: Text)
        , "inputRequests" .= requests
        , "requestState" .= sealed
        ]
  where
    internalError message = JsonRpcError
      { errorCode = -32603
      , errorMessage = "Internal error: " <> message
      , errorData = Nothing
      }

openPayload :: StateCodec -> Text -> Maybe Value -> ClientContext -> Text
            -> IO (Either JsonRpcError ([Text], Maybe Text))
openPayload codec method params ctx sealed = do
  opened <- openState codec sealed
  now <- getPOSIXTime
  let check = do
        bytes <- case opened of
          Left StateUnknownKey -> Left (unknownKey, "unknown key id")
          Left StateInvalid    -> Left (invalid, "could not be opened")
          Right b              -> Right b
        payload <- either (\e -> Left (invalid, "bad payload: " <> T.pack e)) Right $
          eitherDecodeStrict bytes
        when (payloadMethod payload /= method) $
          Left (invalid, "issued for " <> payloadMethod payload <> ", presented on " <> method)
        when (payloadParams payload /= paramsDigest params) $
          Left (invalid, "issued for different request parameters")
        when (payloadPrincipal payload /= principalDigest ctx) $
          Left (invalid, "issued to a different principal")
        when (fromInteger (payloadExpires payload) < now) $
          Left (expired, "expired")
        pure (payloadKeys payload, payloadState payload)
  case check of
    Right issued -> pure (Right issued)
    Left (err, reason) -> do
      hPutStrLn stderr $ "Rejected requestState on " <> T.unpack method <> ": " <> T.unpack reason
      pure (Left err)
  where
    invalid = invalidParams "Invalid requestState"
    expired = invalidParams "requestState has expired: retry the request without it"
    unknownKey = invalidParams
      "requestState was issued by another server instance or under a retired key"

invalidParams :: Text -> JsonRpcError
invalidParams message = JsonRpcError
  { errorCode = -32602
  , errorMessage = message
  , errorData = Nothing
  }

-- | Digest of the request parameters that define the request: everything
-- but the round's own fields and @_meta@.
paramsDigest :: Maybe Value -> Text
paramsDigest params = digest $ case params of
  Just (Object o) -> Object (foldr KM.delete o ["inputResponses", "requestState", "_meta"])
  Just v          -> v
  Nothing         -> Null

principalDigest :: ClientContext -> Text
principalDigest = digest . maybe Null id . clientPrincipal

digest :: Value -> Text
digest v = TE.decodeUtf8 $ B64.encode $ BA.convert
  (Hash.hashlazy (canonicalJson v) :: Hash.Digest Hash.SHA256)
