{-# LANGUAGE OverloadedStrings #-}

-- | Golden wire-format fixtures: a canned request set is run through
-- 'handleMcpMessage' and each response is compared against a checked-in
-- fixture under @test/golden/@.
--
-- The legacy fixtures were generated from @main@ at v0.2.0 (commit
-- @7bd1bcc@), so they anchor the promise that dual-era support leaves
-- legacy responses unchanged. The modern fixtures pin the 2026-07-28
-- envelope introduced by this revision of the library.
--
-- Responses are compared as parsed 'Value's, not raw bytes: aeson's object
-- key order depends on the aeson\/hashable versions in the build plan, which
-- vary across the CI matrix, while 'Value' equality is stable and still
-- catches every added, removed, renamed or changed field.
--
-- To create a fixture for a new case, run the suite with @GOLDEN_ACCEPT=1@:
-- missing fixture files are then written from the current output. Existing
-- fixtures are never overwritten — delete one first to regenerate it, and
-- never regenerate the legacy fixtures from a branch that intends to keep
-- legacy output unchanged.
module Spec.GoldenWire (spec) where

import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server
import MCP.Server.Handlers (handleMcpMessage)
import MCP.Server.JsonRpc
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Test.Hspec

goldenServerInfo :: McpServerInfo
goldenServerInfo = McpServerInfo
  { serverName = "Golden Server"
  , serverVersion = "1.0.0"
  , serverInstructions = "Golden fixture server"
  }

-- Deterministic manual handlers (no Template Haskell): one prompt, one
-- resource, one happy tool, one failing tool.
goldenHandlers :: McpServerHandlers
goldenHandlers = McpServerHandlers
  { prompts = Just (promptList, promptGet)
  , resources = Just (resourceList, resourceRead)
  , tools = Just (toolList, toolCall)
  }
  where
    promptList _ = pure
      [ PromptDefinition "greet" "Greet someone"
          [ArgumentDefinition "name" "Who to greet" True] Nothing
      ]
    promptGet _ name args = case name of
      "greet" -> pure $ Right $ PromptResult (Just "A greeting")
        [PromptMessage RoleUser (ContentText ("Hello, " <> Map.findWithDefault "?" "name" args <> "!"))]
      _ -> pure $ Left $ InvalidPromptName name

    resourceList _ = pure
      [ ResourceDefinition "resource://info" "info" (Just "Some info") (Just "text/plain") Nothing ]
    resourceRead _ uri
      | show uri == "resource://info" = pure $ Right $ ResourceText uri "text/plain" "The golden info"
      | otherwise = pure $ Left $ ResourceNotFound $ T.pack $ show uri

    toolList _ = pure
      [ ToolDefinition "echo" "Echo the text"
          (Schema Nothing (SchemaObject
            [("text", Schema (Just "The text") (SchemaString Nothing))] ["text"]))
          Nothing Nothing
      ]
    toolCall _ name args = case name of
      "echo" -> case Map.lookup "text" args of
        Just (String t) -> pure $ Right $ toolResult [ContentText ("echo: " <> t)]
        _               -> pure $ Left $ MissingRequiredParams "field 'text' is missing"
      "boom" -> pure $ Right $ toolError "kaboom"
      _      -> pure $ Left $ UnknownTool name

-- | (fixture path, raw request). The raw requests are written out verbatim
-- so the fixtures capture the full request->response wire behavior.
cases :: [(FilePath, BSL.ByteString)]
cases =
  -- Legacy era: no _meta; served under the initialize-negotiated revision.
  [ ("legacy/initialize",         "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"golden-client\",\"version\":\"1.0\"}}}")
  , ("legacy/ping",               "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}")
  , ("legacy/tools-list",         "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/list\"}")
  , ("legacy/tools-call-echo",    "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"arguments\":{\"text\":\"hi\"}}}")
  , ("legacy/tools-call-unknown", "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\"name\":\"nope\",\"arguments\":{}}}")
  , ("legacy/tools-call-boom",    "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"boom\",\"arguments\":{}}}")
  , ("legacy/prompts-list",       "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"prompts/list\"}")
  , ("legacy/prompts-get",        "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"prompts/get\",\"params\":{\"name\":\"greet\",\"arguments\":{\"name\":\"World\"}}}")
  , ("legacy/resources-list",     "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"resources/list\"}")
  , ("legacy/resources-read",     "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"resources/read\",\"params\":{\"uri\":\"resource://info\"}}")

  -- Modern era: _meta declares 2026-07-28; responses carry the modern
  -- envelope (resultType, serverInfo _meta, cacheability on list/read).
  , ("modern/server-discover",     "{\"jsonrpc\":\"2.0\",\"id\":21,\"method\":\"server/discover\",\"params\":{" <> meta <> "}}")
  , ("modern/tools-list",          "{\"jsonrpc\":\"2.0\",\"id\":22,\"method\":\"tools/list\",\"params\":{" <> meta <> "}}")
  , ("modern/tools-call-echo",     "{\"jsonrpc\":\"2.0\",\"id\":23,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"arguments\":{\"text\":\"hi\"}," <> meta <> "}}")
  , ("modern/prompts-get",         "{\"jsonrpc\":\"2.0\",\"id\":24,\"method\":\"prompts/get\",\"params\":{\"name\":\"greet\",\"arguments\":{\"name\":\"World\"}," <> meta <> "}}")
  , ("modern/resources-read",      "{\"jsonrpc\":\"2.0\",\"id\":25,\"method\":\"resources/read\",\"params\":{\"uri\":\"resource://info\"," <> meta <> "}}")
  , ("modern/unsupported-version", "{\"jsonrpc\":\"2.0\",\"id\":26,\"method\":\"tools/list\",\"params\":{\"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2099-01-01\"}}}")
  , ("modern/initialize",          "{\"jsonrpc\":\"2.0\",\"id\":27,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2026-07-28\",\"capabilities\":{},\"clientInfo\":{\"name\":\"golden-client\",\"version\":\"1.0\"}," <> meta <> "}}")
  , ("modern/ping",                "{\"jsonrpc\":\"2.0\",\"id\":28,\"method\":\"ping\",\"params\":{" <> meta <> "}}")
  ]
  where
    meta = "\"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\",\"io.modelcontextprotocol/clientInfo\":{\"name\":\"golden-client\",\"version\":\"1.0\"},\"io.modelcontextprotocol/clientCapabilities\":{}}"

runCase :: BSL.ByteString -> IO Value
runCase raw = do
  jsonValue <- either (fail . ("request does not parse: " ++)) pure (eitherDecode raw)
  message <- either (fail . ("request is not JSON-RPC: " ++)) pure (parseJsonRpcMessage jsonValue)
  maybeResponse <- handleMcpMessage goldenServerInfo defaultCacheHints goldenHandlers anonymousContext message
  case maybeResponse of
    Just responseMsg -> pure $ encodeJsonRpcMessage responseMsg
    Nothing          -> fail "expected a response"

spec :: Spec
spec = describe "Golden wire-format fixtures" $
  forM_ cases $ \(name, raw) ->
    it name $ do
      actual <- runCase raw
      let path = "test/golden/" ++ name ++ ".json"
      exists <- doesFileExist path
      accept <- lookupEnv "GOLDEN_ACCEPT"
      if not exists && accept /= Nothing
        then BSL.writeFile path (encode actual)
        else do
          fixtureBytes <- BSL.readFile path
          case eitherDecode fixtureBytes :: Either String Value of
            Left err      -> expectationFailure $ "fixture does not parse: " ++ err
            Right fixture -> actual `shouldBe` fixture
