{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Spec.SchemaValidation (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server
import MCP.Server.Derive
import Test.Hspec
import TestTypes
import TestData

-- Generate handlers for testing
testToolHandlers :: (ToolListHandler IO, ToolCallHandler IO)
testToolHandlers = $(deriveToolHandler ''TestTool 'handleTestTool)

testToolHandlersWithDescriptions :: (ToolListHandler IO, ToolCallHandler IO)
testToolHandlersWithDescriptions = $(deriveToolHandlerWithDescription ''TestTool 'handleTestTool testDescriptions)

-- Helper functions for schema validation
findTool :: Text -> [ToolDefinition] -> ToolDefinition
findTool toolName toolDefs =
  case filter (\def -> toolDefinitionName def == toolName) toolDefs of
    [def] -> def
    [] -> error $ "Tool not found: " ++ T.unpack toolName
    _ -> error $ "Multiple tools found with name: " ++ T.unpack toolName

getProperty :: Text -> ToolDefinition -> InputSchemaDefinitionProperty
getProperty propertyName toolDef =
  case toolDefinitionInputSchema toolDef of
    InputSchemaDefinitionObject props _ ->
      case lookup propertyName props of
        Just prop -> prop
        Nothing -> error $ "Property not found: " ++ T.unpack propertyName

assertToolExists :: Text -> [ToolDefinition] -> IO ()
assertToolExists toolName toolDefs = do
  toolDefs `shouldSatisfy` any (\def -> toolDefinitionName def == toolName)

assertHasProperty :: Text -> Text -> ToolDefinition -> IO ()
assertHasProperty propertyName expectedType toolDef = do
  let prop = getProperty propertyName toolDef
  propertyType prop `shouldBe` expectedType

assertPropertyDescription :: Text -> ToolDefinition -> Text -> IO ()
assertPropertyDescription propertyName toolDef expectedDesc = do
  let prop = getProperty propertyName toolDef
  propertyDescription prop `shouldBe` expectedDesc

assertRequiredFields :: [Text] -> ToolDefinition -> IO ()
assertRequiredFields expectedFields toolDef = do
  case toolDefinitionInputSchema toolDef of
    InputSchemaDefinitionObject _ required ->
      all (`elem` required) expectedFields `shouldBe` True

assertToolDescription :: ToolDefinition -> Text -> IO ()
assertToolDescription toolDef expectedDesc =
  toolDefinitionDescription toolDef `shouldBe` expectedDesc

spec :: Spec
spec = describe "Schema Validation and Custom Descriptions" $ do

  describe "Schema Generation" $ do
    let (listHandler, _) = testToolHandlers

    forM_ schemaTestCases $ \testCase -> do
      it (T.unpack $ schemaTestDescription testCase) $ do
        toolDefs <- listHandler

        assertToolExists (schemaToolName testCase) toolDefs
        let toolDef = findTool (schemaToolName testCase) toolDefs

        -- Validate property types
        forM_ (expectedProperties testCase) $ \(propName, expectedType) ->
          assertHasProperty propName expectedType toolDef

        -- Validate required fields
        assertRequiredFields (requiredFields testCase) toolDef

  describe "Custom Descriptions" $ do
    it "applies correct tool descriptions" $ do
      let (listHandler, _) = testToolHandlersWithDescriptions
      toolDefs <- listHandler

      assertToolExists "echo" toolDefs
      let echoDef = findTool "echo" toolDefs
      assertToolDescription echoDef "Echoes the input text back to the user"

      assertToolExists "calculate" toolDefs
      let calculateDef = findTool "calculate" toolDefs
      assertToolDescription calculateDef "Performs mathematical calculations"

    it "applies correct field descriptions for Calculate tool" $ do
      let (listHandler, _) = testToolHandlersWithDescriptions
      toolDefs <- listHandler

      assertToolExists "calculate" toolDefs
      let calculateDef = findTool "calculate" toolDefs

      assertPropertyDescription "operation" calculateDef "The mathematical operation to perform"
      assertPropertyDescription "x" calculateDef "The first number"
      assertPropertyDescription "y" calculateDef "The second number"
