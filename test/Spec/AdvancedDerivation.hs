{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Spec.AdvancedDerivation (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server
import MCP.Server.Derive
import Test.Hspec
import TestHelpers
import TestTypes
import TestData

-- Generate advanced handlers for separate parameter types
testSeparateParamsToolHandlers :: (ToolListHandler, ToolCallHandler)
testSeparateParamsToolHandlers = $(deriveToolHandler ''SeparateParamsTool 'handleSeparateParamsTool)

testRecursiveToolHandlers :: (ToolListHandler, ToolCallHandler)
testRecursiveToolHandlers = $(deriveToolHandler ''RecursiveTool 'handleRecursiveTool)

testSeparateParamsToolHandlersWithDescriptions :: (ToolListHandler, ToolCallHandler)
testSeparateParamsToolHandlersWithDescriptions = $(deriveToolHandlerWithDescription ''SeparateParamsTool 'handleSeparateParamsTool separateParamsDescriptions)

testRecursiveToolHandlersWithDescriptions :: (ToolListHandler, ToolCallHandler)
testRecursiveToolHandlersWithDescriptions = $(deriveToolHandlerWithDescription ''RecursiveTool 'handleRecursiveTool separateParamsDescriptions)

-- Helper functions for advanced testing
findToolByName :: Text -> [ToolDefinition] -> ToolDefinition
findToolByName toolName toolDefs =
  case filter (\def -> toolDefinitionName def == toolName) toolDefs of
    [def] -> def
    [] -> error $ "Tool not found: " ++ T.unpack toolName
    _ -> error $ "Multiple tools found with name: " ++ T.unpack toolName

assertSchemaHasProperties :: [Text] -> ToolDefinition -> IO ()
assertSchemaHasProperties expectedProps toolDef = do
  let propNames = map fst (schemaProps (toolDefinitionInputSchema toolDef))
  all (`elem` propNames) expectedProps `shouldBe` True

assertToolCallResult :: ToolCallHandler -> Text -> [(Text, Text)] -> Text -> IO ()
assertToolCallResult handler toolName args expectedContent = do
  result <- handler anonCtx toolName (stringArgs args)
  result `shouldBeTextResult` expectedContent

assertToolHasDescription :: Text -> Text -> ToolDefinition -> IO ()
assertToolHasDescription toolName expectedDesc toolDef = do
  toolDefinitionName toolDef `shouldBe` toolName
  toolDefinitionDescription toolDef `shouldBe` expectedDesc

assertPropertyHasDescription :: Text -> Text -> ToolDefinition -> IO ()
assertPropertyHasDescription propName expectedDesc toolDef = do
  case lookupProp propName toolDef of
    Just prop -> schemaDescription prop `shouldBe` Just expectedDesc
    Nothing -> expectationFailure $ "Property not found: " ++ T.unpack propName

spec :: Spec
spec = describe "Advanced Template Haskell Derivation" $ do

  describe "Separate Parameter Types" $ do
    it "generates correct schema for separate parameter tools" $ do
      let (listHandler, _) = testSeparateParamsToolHandlers
      toolDefs <- listHandler anonCtx

      -- Test GetValue tool schema
      let getValueDef = findToolByName "get_value" toolDefs
      assertSchemaHasProperties ["_gvpKey"] getValueDef

      -- Test SetValue tool schema
      let setValueDef = findToolByName "set_value" toolDefs
      assertSchemaHasProperties ["_svpKey", "_svpValue"] setValueDef

    forM_ separateParamsTestCases $ \testCase ->
      it (T.unpack $ sepTestDescription testCase) $ do
        let (_, callHandler) = testSeparateParamsToolHandlers
        assertToolCallResult callHandler (sepToolName testCase) (sepArgs testCase) (sepExpectedResult testCase)

  describe "Recursive Parameter Types" $ do
    it "generates correct schema for recursive parameter tools" $ do
      let (listHandler, _) = testRecursiveToolHandlers
      toolDefs <- listHandler anonCtx

      let processDataDef = findToolByName "process_data" toolDefs
      assertSchemaHasProperties ["_ipName", "_ipAge"] processDataDef

    forM_ recursiveParamsTestCases $ \testCase ->
      it (T.unpack $ recTestDescription testCase) $ do
        let (_, callHandler) = testRecursiveToolHandlers
        assertToolCallResult callHandler (recToolName testCase) (recArgs testCase) (recExpectedResult testCase)

  describe "Custom Descriptions with Separate Parameters" $ do
    it "applies correct tool descriptions for separate parameter tools" $ do
      let (listHandler, _) = testSeparateParamsToolHandlersWithDescriptions
      toolDefs <- listHandler anonCtx

      let getValueDef = findToolByName "get_value" toolDefs
      assertToolHasDescription "get_value" "Retrieves a value from the key-value store" getValueDef

      let setValueDef = findToolByName "set_value" toolDefs
      assertToolHasDescription "set_value" "Sets a value in the key-value store" setValueDef

    it "applies correct field descriptions for separate parameter tools" $ do
      let (listHandler, _) = testSeparateParamsToolHandlersWithDescriptions
      toolDefs <- listHandler anonCtx

      let getValueDef = findToolByName "get_value" toolDefs
      assertPropertyHasDescription "_gvpKey" "The key to retrieve the value for" getValueDef

      let setValueDef = findToolByName "set_value" toolDefs
      assertPropertyHasDescription "_svpKey" "The key to set the value for" setValueDef
      assertPropertyHasDescription "_svpValue" "The value to store" setValueDef

  describe "Recursive Tool Descriptions" $ do
    it "applies correct descriptions for recursive parameter tools" $ do
      let (listHandler, _) = testRecursiveToolHandlersWithDescriptions
      toolDefs <- listHandler anonCtx

      let processDataDef = findToolByName "process_data" toolDefs
      assertToolHasDescription "process_data" "Processes user data with age validation" processDataDef

    it "applies correct field descriptions for recursive parameters" $ do
      let (listHandler, _) = testRecursiveToolHandlersWithDescriptions
      toolDefs <- listHandler anonCtx

      let processDataDef = findToolByName "process_data" toolDefs
      assertPropertyHasDescription "_ipName" "The person's full name" processDataDef
      assertPropertyHasDescription "_ipAge" "The person's age in years" processDataDef
