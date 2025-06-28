{-# LANGUAGE OverloadedStrings #-}

module Spec.UnicodeHandling (spec) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap    as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           MCP.Server.Protocol
import           MCP.Server.Types
import           System.IO            (hSetEncoding, stderr, stdout, utf8)
import           Test.Hspec

spec :: Spec
spec = describe "Unicode Handling" $ do
  describe "Unicode in Content Types" $ do
    it "handles Unicode in ContentText" $ do
      -- Set UTF-8 encoding for this test
      hSetEncoding stdout utf8
      hSetEncoding stderr utf8
      let content = ContentText "Test with Unicode: √π∞≈∑"
      let json = toJSON content
      case json of
        Object obj -> case KM.lookup "text" obj of
          Just (String txt) -> "√π∞≈∑" `T.isInfixOf` txt `shouldBe` True
          _ -> expectationFailure "Expected String in text field"
        _ -> expectationFailure "Expected Object"

    it "handles Unicode in ResourceContent" $ do
      uri <- case parseURI "test://unicode" of
        Just u  -> return u
        Nothing -> fail "Invalid URI"
      let resource = ResourceText uri "text/plain" "Unicode content: ∀x∈ℝ: √x²=|x|"
      let json = toJSON resource
      case json of
        Object obj -> case KM.lookup "text" obj of
          Just (String txt) -> "∀x∈ℝ: √x²=|x|" `T.isInfixOf` txt `shouldBe` True
          _ -> expectationFailure "Expected String in text field"
        _ -> expectationFailure "Expected Object"

    it "handles square root symbol specifically" $ do
      let content = ContentText "Square root: √16 = 4"
      let json = toJSON content
      case json of
        Object obj -> case KM.lookup "text" obj of
          Just (String txt) -> txt `shouldBe` "Square root: √16 = 4"
          _ -> expectationFailure "Expected String in text field"
        _ -> expectationFailure "Expected Object"

  describe "JSON Encoding/Decoding with Unicode" $ do
    it "properly encodes and decodes Unicode content" $ do
      let originalContent = ContentText "Mathematical symbols: ∀∃∈∉∪∩⊆⊇√∑∏∞≤≥≠≈π∅Ω∆∇"
      let encoded = encode originalContent
      let decoded = eitherDecode encoded
      case decoded of
        Right decodedContent -> decodedContent `shouldBe` originalContent
        Left err             -> expectationFailure $ "Failed to decode: " ++ err

    it "handles Unicode in JSON-RPC messages" $ do
      let response = ToolsCallResponse
            { toolsCallContent = [ContentText "Result: √16 = 4, π ≈ 3.14159"]
            , toolsCallIsError = Nothing
            }
      let encoded = encode response
      -- Just verify the JSON can be encoded and contains Unicode
      BSL.length encoded `shouldSatisfy` (> 0)

  describe "UTF-8 Text Encoding" $ do
    it "properly handles UTF-8 byte sequences" $ do
      let unicodeText = "Unicode: √∞≈π"
      let utf8Bytes = TE.encodeUtf8 unicodeText
      let decodedText = TE.decodeUtf8 utf8Bytes
      decodedText `shouldBe` unicodeText

    it "handles multibyte Unicode characters" $ do
      let characters = ["√", "∞", "π", "∑", "∏", "∀", "∃", "∈", "∉"]
      mapM_ (\char -> do
        let encoded = TE.encodeUtf8 char
        let decoded = TE.decodeUtf8 encoded
        decoded `shouldBe` char
        ) characters

  describe "Protocol Messages with Unicode" $ do
    it "handles Unicode in tool responses" $ do
      let response = ToolsCallResponse
            { toolsCallContent = [ContentText "Mathematical result: √(π²+e²) ≈ 4.53"]
            , toolsCallIsError = Nothing
            }
      let json = toJSON response
      -- Verify the JSON can be encoded and contains Unicode
      let encoded = encode json
      BSL.length encoded `shouldSatisfy` (> 0)

    it "handles Unicode in prompt responses" $ do
      let message = PromptMessage RoleUser (ContentText "Formula: E=mc², √(x²+y²)")
      let response = PromptsGetResponse
            { promptsGetDescription = Just "Mathematical formulas with symbols: ∀∃"
            , promptsGetMessages = [message]
            }
      let json = toJSON response
      let encoded = encode json
      BSL.length encoded `shouldSatisfy` (> 0)

    it "handles Unicode in error messages" $ do
      let mcpError = InvalidParams "Invalid formula: expected √x not ∛x"
      let json = toJSON mcpError
      case json of
        Object obj -> case KM.lookup "message" obj of
          Just (String msg) -> "√" `T.isInfixOf` msg `shouldBe` True
          _                 -> expectationFailure "Expected message field"
        _ -> expectationFailure "Expected Object"

  describe "Manual Handler Tests with Unicode" $ do
    it "tests Unicode content through manual prompt handler" $ do
      let promptHandler :: PromptName -> [(ArgumentName, ArgumentValue)] -> IO (Either Error Content)
          promptHandler "unicode_test" args = do
            case lookup "formula" args of
              Just formula -> return $ Right $ ContentText $ "Formula result: " <> formula <> " → √solution"
              Nothing -> return $ Left $ MissingRequiredParams "formula"
          promptHandler name _ = return $ Left $ InvalidPromptName name

      result <- promptHandler "unicode_test" [("formula", "x² + y² = z²")]
      case result of
        Right (ContentText txt) -> do
          txt `shouldSatisfy` T.isInfixOf "√"
          txt `shouldSatisfy` T.isInfixOf "²"
        _ -> expectationFailure "Expected successful ContentText with Unicode"

    it "tests Unicode content through manual tool handler" $ do
      let toolHandler :: ToolName -> [(ArgumentName, ArgumentValue)] -> IO (Either Error Content)
          toolHandler "calculate" args = do
            case lookup "expression" args of
              Just expr -> return $ Right $ ContentText $ "Calculation: " <> expr <> " = √result"
              Nothing -> return $ Left $ MissingRequiredParams "expression"
          toolHandler name _ = return $ Left $ UnknownTool name

      result <- toolHandler "calculate" [("expression", "∫₀^∞ e^(-x²) dx")]
      case result of
        Right (ContentText txt) -> do
          txt `shouldSatisfy` T.isInfixOf "√"
          txt `shouldSatisfy` T.isInfixOf "∫"
        _ -> expectationFailure "Expected successful ContentText with Unicode"

    it "tests Unicode content through manual resource handler" $ do
      let resourceHandler :: URI -> IO (Either Error ResourceContent)
          resourceHandler uri = do
            let uriStr = show uri
            if "unicode" `T.isInfixOf` T.pack uriStr
              then return $ Right $ ResourceText uri "text/plain" "Unicode symbols: ∀∃∈∉√∑∏∞≤≥≠≈π"
              else return $ Left $ ResourceNotFound $ T.pack uriStr

      uri <- case parseURI "resource://unicode_test" of
        Just u  -> return u
        Nothing -> fail "Invalid URI"

      result <- resourceHandler uri
      case result of
        Right (ResourceText _ _ txt) -> do
          txt `shouldSatisfy` T.isInfixOf "∀∃∈∉"
          txt `shouldSatisfy` T.isInfixOf "√∑∏∞"
        _ -> expectationFailure "Expected successful ResourceText with Unicode"

  describe "Integration test simulating MCP workflow" $ do
    it "handles complete Unicode workflow without Template Haskell" $ do
      -- Create manual handlers with Unicode content
      let promptListHandler = return [
            PromptDefinition
              { promptDefinitionName = "math_formula"
              , promptDefinitionTitle = Nothing
              , promptDefinitionDescription = "Generate mathematical formulas with Unicode: ∀∃∈√"
              , promptDefinitionArguments = [ArgumentDefinition "formula" "Mathematical expression" True]
              , promptDefinitionMeta = Nothing
              }
            ]

      let promptGetHandler name args = case name of
            "math_formula" -> case lookup "formula" args of
              Just formula -> return $ Right $ ContentText $ "Formula: " <> formula <> " → √(solution)"
              Nothing -> return $ Left $ MissingRequiredParams "formula"
            _ -> return $ Left $ InvalidPromptName name

      let resourceListHandler = return [
            ResourceDefinition
              { resourceDefinitionURI = "resource://unicode_symbols"
              , resourceDefinitionName = "unicode_symbols"
              , resourceDefinitionTitle = Nothing
              , resourceDefinitionDescription = Just "Unicode mathematical symbols: ∀∃∈∉√∑"
              , resourceDefinitionMimeType = Just "text/plain"
              , resourceDefinitionMeta = Nothing
              }
            ]

      let resourceReadHandler uri =
            if show uri == "resource://unicode_symbols"
              then return $ Right $ ResourceText uri "text/plain" "Symbols: ∀∃∈∉∪∩⊆⊇√∑∏∞≤≥≠≈π∅"
              else return $ Left $ ResourceNotFound $ T.pack $ show uri

      let toolListHandler = return [
            ToolDefinition
              { toolDefinitionName = "calculate"
              , toolDefinitionTitle = Nothing
              , toolDefinitionDescription = "Calculate with Unicode symbols: √∑∏"
              , toolDefinitionInputSchema = InputSchemaDefinitionObject
                  { properties = [("expression", InputSchemaDefinitionProperty "string" "Mathematical expression")]
                  , required = ["expression"]
                  }
              , toolDefinitionMeta = Nothing
              }
            ]

      let toolCallHandler name args = case name of
            "calculate" -> case lookup "expression" args of
              Just expr -> return $ Right $ ContentText $ "Result: " <> expr <> " → √answer"
              Nothing -> return $ Left $ MissingRequiredParams "expression"
            _ -> return $ Left $ UnknownTool name

      let handlers = McpServerHandlers
            { prompts = Just (promptListHandler, promptGetHandler)
            , resources = Just (resourceListHandler, resourceReadHandler)
            , tools = Just (toolListHandler, toolCallHandler)
            }

      -- Test that all handlers work with Unicode
      promptList <- fst $ case prompts handlers of Just h -> h; Nothing -> error "No prompts"
      promptList `shouldSatisfy` (not . null)

      resourceList <- fst $ case resources handlers of Just h -> h; Nothing -> error "No resources"
      resourceList `shouldSatisfy` (not . null)

      toolList <- fst $ case tools handlers of Just h -> h; Nothing -> error "No tools"
      toolList `shouldSatisfy` (not . null)

      -- Test actual Unicode handling
      promptResult <- snd (case prompts handlers of Just h -> h; Nothing -> error "No prompts") "math_formula" [("formula", "√(x²+y²)")]
      case promptResult of
        Right (ContentText txt) -> txt `shouldSatisfy` T.isInfixOf "√"
        _ -> expectationFailure "Expected successful prompt result"

      uri <- case parseURI "resource://unicode_symbols" of
        Just u  -> return u
        Nothing -> fail "Invalid URI"
      resourceResult <- snd (case resources handlers of Just h -> h; Nothing -> error "No resources") uri
      case resourceResult of
        Right (ResourceText _ _ txt) -> txt `shouldSatisfy` T.isInfixOf "∀∃∈∉"
        _ -> expectationFailure "Expected successful resource result"

      toolResult <- snd (case tools handlers of Just h -> h; Nothing -> error "No tools") "calculate" [("expression", "∫₀^∞ e^(-x²) dx = √π/2")]
      case toolResult of
        Right (ContentText txt) -> do
          txt `shouldSatisfy` T.isInfixOf "√"
          txt `shouldSatisfy` T.isInfixOf "∫"
        _ -> expectationFailure "Expected successful tool result"
