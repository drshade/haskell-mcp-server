{-# LANGUAGE OverloadedStrings #-}

-- | A handler that throws (rather than returning an error value) must still
-- produce a response for its request id: a -32603 internal error. Without
-- this the exception escapes the transport and the client waits forever.
-- Asynchronous exceptions are deliberately not covered here — cancellation
-- relies on them propagating, which "Spec.Cancellation" pins down.
module Spec.HandlerExceptions (spec) where

import Control.Exception (throwIO, ErrorCall (..))
import Data.Aeson
import qualified Data.Text as T
import MCP.Server
import MCP.Server.Handlers (handleMcpMessage)
import MCP.Server.JsonRpc
import Test.Hspec

throwingServer :: McpServerHandlers
throwingServer = noHandlers
  { tools = Just
      ( \_ -> pure []
      , \_ _ _ -> throwIO (ErrorCall "tool exploded")
      )
  , prompts = Just
      ( \_ -> pure []
      , \_ _ _ -> error "prompt exploded"
      )
  }

call :: T.Text -> Value -> IO (Maybe JsonRpcMessage)
call method params =
  handleMcpMessage (McpServerInfo "T" "1" "") defaultCacheHints
    noNotificationSupport (\_ -> pure ()) throwingServer anonymousContext
    (JsonRpcMessageRequest (JsonRpcRequest "2.0" (RequestIdNumber 7) method (Just params)))

errorOf :: Maybe JsonRpcMessage -> (RequestId, Int, T.Text)
errorOf (Just (JsonRpcMessageResponse r)) = case responseError r of
  Just e  -> (responseId r, errorCode e, errorMessage e)
  Nothing -> error $ "expected an error response, got " ++ show (responseResult r)
errorOf other = error $ "expected a response, got " ++ show other

spec :: Spec
spec = describe "Handler exceptions" $ do
  it "a throwing tool handler yields -32603 for its request id" $ do
    (rid, code, msg) <- errorOf <$> call "tools/call" (object ["name" .= ("x" :: T.Text), "arguments" .= object []])
    rid `shouldBe` RequestIdNumber 7
    code `shouldBe` (-32603)
    msg `shouldSatisfy` T.isInfixOf "tool exploded"

  it "a throwing prompt handler yields -32603 (pure error, forced inside the handler)" $ do
    (rid, code, msg) <- errorOf <$> call "prompts/get" (object ["name" .= ("x" :: T.Text), "arguments" .= object []])
    rid `shouldBe` RequestIdNumber 7
    code `shouldBe` (-32603)
    msg `shouldSatisfy` T.isInfixOf "prompt exploded"
