{-# LANGUAGE OverloadedStrings #-}

module Spec.VersionNegotiation (spec) where

import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server.Handlers (validateProtocolVersion)
import MCP.Server.Protocol (protocolVersion)
import MCP.Server.Types
import Test.Hspec

spec :: Spec
spec = describe "Protocol Version Negotiation" $ do
  describe "validateProtocolVersion" $ do
    it "accepts 2025-06-18 (latest)" $ do
      validateProtocolVersion "2025-06-18" `shouldBe` Right "2025-06-18"
    
    it "accepts 2025-03-26 (previous)" $ do
      validateProtocolVersion "2025-03-26" `shouldBe` Right "2025-03-26"
    
    it "accepts 2024-11-05 (legacy)" $ do
      validateProtocolVersion "2024-11-05" `shouldBe` Right "2024-11-05"
    
    it "rejects unknown versions" $ do
      case validateProtocolVersion "2023-01-01" of
        Left err -> T.unpack err `shouldContain` "Unsupported protocol version"
        Right _ -> expectationFailure "Should have rejected unknown version"
    
    it "rejects empty version" $ do
      case validateProtocolVersion "" of
        Left err -> T.unpack err `shouldContain` "Unsupported protocol version"
        Right _ -> expectationFailure "Should have rejected empty version"

  describe "protocolVersion constant" $ do
    it "is set to 2025-06-18" $ do
      protocolVersion `shouldBe` "2025-06-18"

  describe "backwards compatibility" $ do
    it "supports all three versions" $ do
      let versions = ["2025-06-18", "2025-03-26", "2024-11-05"]
          isRight (Right _) = True
          isRight (Left _) = False
      mapM_ (\v -> validateProtocolVersion v `shouldSatisfy` isRight) versions

  describe "new fields" $ do
    it "creates PromptDefinition with new fields" $ do
      let prompt = PromptDefinition
            { promptDefinitionName = "test"
            , promptDefinitionTitle = Just "Test Prompt"
            , promptDefinitionDescription = "A test prompt"
            , promptDefinitionArguments = []
            , promptDefinitionMeta = Just (object ["version" .= ("1.0" :: Text)])
            }
      promptDefinitionName prompt `shouldBe` "test"
      promptDefinitionTitle prompt `shouldBe` Just "Test Prompt"
      promptDefinitionMeta prompt `shouldNotBe` Nothing
    
    it "creates ResourceDefinition with new fields" $ do
      let resource = ResourceDefinition
            { resourceDefinitionURI = "resource://test"
            , resourceDefinitionName = "test"
            , resourceDefinitionTitle = Just "Test Resource"
            , resourceDefinitionDescription = Just "A test resource"
            , resourceDefinitionMimeType = Just "text/plain"
            , resourceDefinitionMeta = Nothing
            }
      resourceDefinitionName resource `shouldBe` "test"
      resourceDefinitionTitle resource `shouldBe` Just "Test Resource"
    
    it "creates ToolDefinition with new fields" $ do
      let tool = ToolDefinition
            { toolDefinitionName = "test"
            , toolDefinitionTitle = Just "Test Tool"
            , toolDefinitionDescription = "A test tool"
            , toolDefinitionInputSchema = InputSchemaDefinitionObject
                { properties = []
                , required = []
                }
            , toolDefinitionMeta = Nothing
            }
      toolDefinitionName tool `shouldBe` "test"
      toolDefinitionTitle tool `shouldBe` Just "Test Tool"