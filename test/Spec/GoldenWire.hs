{-# LANGUAGE OverloadedStrings #-}

-- | Golden wire-format conformance corpus: @test/golden/@ holds, per case,
-- a JSON-RPC request (@\<name\>.request.json@) and the reference server's
-- exact response (@\<name\>.response.json@), enumerated by
-- @test/golden/manifest.json@. This spec replays every request through
-- 'handleMcpMessage' and compares the response against the fixture.
--
-- The corpus is deliberately API-agnostic — request and response are plain
-- wire bytes, so any MCP implementation that reproduces the reference
-- server described in @test/golden/README.md@ can consume it. Within this
-- library it pins two promises: the legacy fixtures were generated from
-- @main@ at v0.2.0 (commit @7bd1bcc@), anchoring "dual-era support leaves
-- legacy responses unchanged", and the modern fixtures pin the 2026-07-28
-- envelope.
--
-- Responses are compared as parsed 'Value's, not raw bytes: aeson's object
-- key order depends on the aeson\/hashable versions in the build plan, which
-- vary across the CI matrix, while 'Value' equality is stable and still
-- catches every added, removed, renamed or changed field.
--
-- To add a case: write the @.request.json@ by hand, add its manifest entry,
-- and run the suite with @GOLDEN_ACCEPT=1@ — a missing response fixture is
-- then written from the current output. Existing fixtures are never
-- overwritten — delete one first to regenerate it, and never regenerate the
-- legacy fixtures from a branch that intends to keep legacy output
-- unchanged.
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
-- resource, one happy tool, one failing tool. Documented for external
-- consumers in test/golden/README.md — keep the two in sync.
goldenHandlers :: McpServerHandlers
goldenHandlers = noHandlers
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

-- | 'goldenHandlers' extended with the handler slots introduced after
-- v0.2.0. Used only for the fixtures of methods that postdate the legacy
-- anchor: extending the main handler set would change the advertised
-- capabilities and perturb the v0.2.0-anchored initialize fixture.
extendedHandlers :: McpServerHandlers
extendedHandlers = goldenHandlers
  { resourceTemplates = Just $ \_ -> pure
      [ ResourceTemplateDefinition "resource://item/{itemId}" "item"
          (Just "An item") (Just "text/plain") Nothing
      ]
  , completions = Just $ \_ _ref _arg partial _ctx -> pure $ Right $
      completionResult (filter (T.isPrefixOf partial) ["alpha", "beta"])
  }

-- | One manifest entry: which case, and which reference-server variant
-- answers it.
data GoldenCase = GoldenCase
  { caseName          :: FilePath
  , caseHandlers      :: Text  -- ^ "base" or "extended"
  , caseNotifications :: Bool
  }

instance FromJSON GoldenCase where
  parseJSON = withObject "GoldenCase" $ \o -> GoldenCase
    <$> o .: "name"
    <*> o .: "handlers"
    <*> o .: "notifications"

newtype Manifest = Manifest [GoldenCase]

instance FromJSON Manifest where
  parseJSON = withObject "Manifest" $ \o -> Manifest <$> o .: "cases"

-- | The corpus enumeration, read while hspec constructs the spec tree.
loadManifest :: IO [GoldenCase]
loadManifest = do
  bytes <- BSL.readFile "test/golden/manifest.json"
  case eitherDecode bytes of
    Left err            -> fail ("test/golden/manifest.json does not parse: " ++ err)
    Right (Manifest cs) -> pure cs

runCase :: GoldenCase -> BSL.ByteString -> IO Value
runCase gc raw = do
  jsonValue <- either (fail . ("request does not parse: " ++)) pure (eitherDecode raw)
  message <- either (fail . ("request is not JSON-RPC: " ++)) pure (parseJsonRpcMessage jsonValue)
  let handlers = if caseHandlers gc == "extended" then extendedHandlers else goldenHandlers
      support = if caseNotifications gc
        then NotificationSupport { supportsLegacyPush = True, supportsListen = True }
        else noNotificationSupport
  maybeResponse <- handleMcpMessage goldenServerInfo defaultCacheHints support handlers anonymousContext message
  case maybeResponse of
    Just responseMsg -> pure $ encodeJsonRpcMessage responseMsg
    Nothing          -> fail "expected a response"

goldenCase :: GoldenCase -> Spec
goldenCase gc =
  it (caseName gc) $ do
    let requestPath = "test/golden/" ++ caseName gc ++ ".request.json"
        responsePath = "test/golden/" ++ caseName gc ++ ".response.json"
    raw <- BSL.readFile requestPath
    actual <- runCase gc raw
    exists <- doesFileExist responsePath
    accept <- lookupEnv "GOLDEN_ACCEPT"
    if not exists && accept /= Nothing
      then BSL.writeFile responsePath (encode actual)
      else do
        fixtureBytes <- BSL.readFile responsePath
        case eitherDecode fixtureBytes :: Either String Value of
          Left err      -> expectationFailure $ "fixture does not parse: " ++ err
          Right fixture -> actual `shouldBe` fixture

spec :: Spec
spec = describe "Golden wire-format fixtures" $ do
  cases <- runIO loadManifest
  forM_ cases goldenCase
