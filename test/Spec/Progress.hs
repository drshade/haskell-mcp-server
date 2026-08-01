{-# LANGUAGE OverloadedStrings #-}

-- | Coverage for progress notifications and per-request client logging
-- (ADR 0007): the reportProgress/logToClient context actions and their
-- _meta-driven gating.
module Spec.Progress (spec) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.IORef
import Data.Text (Text)
import MCP.Server
import MCP.Server.Handlers (handleMcpMessage)
import MCP.Server.JsonRpc
import Test.Hspec

progressServer :: McpServerHandlers
progressServer = noHandlers
  { tools = Just
      ( \_ -> pure []
      , \ctx name _ -> case name of
          "work" -> do
            reportProgress ctx 0.2 (Just 1.0) (Just "starting")
            reportProgress ctx 0.9 Nothing Nothing
            pure $ Right $ toToolResult ("done" :: Text)
          "chatty" -> do
            logToClient ctx LogInfo (String "fyi")
            logToClient ctx LogError (String "bad")
            pure $ Right $ toToolResult ("ok" :: Text)
          _ -> pure $ Left $ UnknownTool name
      )
  }

-- Run a tools/call with the given _meta fields, collecting emitted
-- request-scoped notifications in order.
callTool :: [Pair] -> Text -> IO ([JsonRpcNotification], Maybe JsonRpcMessage)
callTool metaFields tool = do
  sink <- newIORef []
  let emit n = modifyIORef' sink (++ [n])
      params = object $
        ["name" .= tool, "arguments" .= object []]
          ++ (if null metaFields then [] else ["_meta" .= object metaFields])
  resp <- handleMcpMessage (McpServerInfo "T" "1" "") defaultCacheHints
    noNotificationSupport emit progressServer anonymousContext
    (JsonRpcMessageRequest (JsonRpcRequest "2.0" (RequestIdNumber 1) "tools/call" (Just params)))
  notifications <- readIORef sink
  pure (notifications, resp)

paramsOf :: JsonRpcNotification -> Object
paramsOf n = case notificationParams n of
  Just (Object o) -> o
  other           -> error $ "notification params not an object: " ++ show other

spec :: Spec
spec = describe "Progress and client logging" $ do

  describe "reportProgress" $ do
    it "emits tagged, ordered notifications/progress before the response" $ do
      (ns, resp) <- callTool ["progressToken" .= ("abc123" :: Text)] "work"
      map notificationMethod ns `shouldBe`
        ["notifications/progress", "notifications/progress"]
      case map paramsOf ns of
        [first', second'] -> do
          KM.lookup "progressToken" first' `shouldBe` Just (String "abc123")
          KM.lookup "progress" first' `shouldBe` Just (Number 0.2)
          KM.lookup "total" first' `shouldBe` Just (Number 1.0)
          KM.lookup "message" first' `shouldBe` Just (String "starting")
          KM.lookup "progress" second' `shouldBe` Just (Number 0.9)
          KM.member "total" second' `shouldBe` False
          KM.member "message" second' `shouldBe` False
        other -> expectationFailure $ "expected two notifications, got: " ++ show (length other)
      resp `shouldSatisfy` (/= Nothing)

    it "preserves integer tokens as integers" $ do
      (ns, _) <- callTool ["progressToken" .= (42 :: Int)] "work"
      map (KM.lookup "progressToken" . paramsOf) ns `shouldBe`
        [Just (Number 42), Just (Number 42)]

    it "is a no-op without a progressToken" $ do
      (ns, resp) <- callTool [] "work"
      ns `shouldBe` []
      resp `shouldSatisfy` (/= Nothing)

  describe "logToClient" $ do
    it "filters below the declared level and emits at or above it" $ do
      (ns, _) <- callTool ["io.modelcontextprotocol/logLevel" .= ("warning" :: Text)] "chatty"
      map notificationMethod ns `shouldBe` ["notifications/message"]
      case ns of
        [n] -> do
          KM.lookup "level" (paramsOf n) `shouldBe` Just (String "error")
          KM.lookup "data" (paramsOf n) `shouldBe` Just (String "bad")
        other -> expectationFailure $ "expected one notification, got: " ++ show (length other)

    it "emits everything at a permissive level" $ do
      (ns, _) <- callTool ["io.modelcontextprotocol/logLevel" .= ("debug" :: Text)] "chatty"
      map (KM.lookup "level" . paramsOf) ns `shouldBe`
        [Just (String "info"), Just (String "error")]

    it "emits nothing when the request declared no logLevel (spec MUST NOT)" $ do
      (ns, _) <- callTool [] "chatty"
      ns `shouldBe` []

    it "treats an unparseable logLevel as absent" $ do
      (ns, _) <- callTool ["io.modelcontextprotocol/logLevel" .= ("loudly" :: Text)] "chatty"
      ns `shouldBe` []