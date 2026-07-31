{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Coverage for the 0.2 typed-argument features: native JSON values,
-- enumerations, lists, nested records, isError results and multi-message
-- prompts.
module Spec.TypedArgs (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server
import MCP.Server.Derive
import Test.Hspec
import TestHelpers
import TestTypes

typedHandlers :: (ToolListHandler, ToolCallHandler)
typedHandlers = $(deriveToolHandler ''TypedTool 'handleTypedTool)

convHandlers :: (PromptListHandler, PromptGetHandler)
convHandlers = $(derivePromptHandler ''ConvPrompt 'handleConvPrompt)

callTyped :: Text -> [(Text, Value)] -> IO (Either Error ToolResult)
callTyped name args = snd typedHandlers anonCtx name (valueArgs args)

toolNamed :: Text -> [ToolDefinition] -> ToolDefinition
toolNamed n defs = case [d | d <- defs, toolDefinitionName d == n] of
  (d:_) -> d
  []    -> error $ "tool not found: " ++ T.unpack n

expectInvalidParams :: Either Error ToolResult -> Text -> Expectation
expectInvalidParams (Left (InvalidParams msg)) expectedSubstring =
  msg `shouldSatisfy` T.isInfixOf expectedSubstring
expectInvalidParams other expectedSubstring =
  expectationFailure $ "Expected InvalidParams containing '" ++ T.unpack expectedSubstring ++ "' but got: " ++ show other

spec :: Spec
spec = describe "Typed tool arguments" $ do

  describe "Enumerations" $ do
    it "decodes enum arguments from strings" $ do
      result <- callTyped "paint" [("color", String "green"), ("brightness", Number 5)]
      result `shouldBeTextResult` "Painting Green at 5"

    it "rejects unknown enum values with the allowed set" $ do
      result <- callTyped "paint" [("color", String "purple"), ("brightness", Number 5)]
      expectInvalidParams result "expected one of: red, green, blue"

    it "generates a string schema with enum values" $ do
      toolDefs <- fst typedHandlers anonCtx
      let paintDef = toolNamed "paint" toolDefs
      case lookupProp "color" paintDef of
        Just prop -> schemaShape prop `shouldBe` SchemaString (Just ["red", "green", "blue"])
        Nothing -> expectationFailure "color property missing"

  describe "Native JSON values" $ do
    it "accepts native numbers" $ do
      result <- callTyped "paint" [("color", String "red"), ("brightness", Number 7)]
      result `shouldBeTextResult` "Painting Red at 7"

    it "still accepts numbers sent as strings (lenient)" $ do
      result <- callTyped "paint" [("color", String "red"), ("brightness", String "7")]
      result `shouldBeTextResult` "Painting Red at 7"

    it "rejects non-integral numbers for Int fields" $ do
      result <- callTyped "paint" [("color", String "red"), ("brightness", Number 7.5)]
      expectInvalidParams result "field 'brightness'"

  describe "Lists" $ do
    it "decodes arrays of integers" $ do
      result <- callTyped "bulk_add" [("values", toJSON [1 :: Int, 2, 3])]
      result `shouldBeTextResult` "Sum: 6"

    it "rejects mistyped array elements" $ do
      result <- callTyped "bulk_add" [("values", toJSON [Number 1, String "x"])]
      expectInvalidParams result "Failed to parse Int"

    it "generates an array schema" $ do
      toolDefs <- fst typedHandlers anonCtx
      let bulkDef = toolNamed "bulk_add" toolDefs
      fmap schemaTypeName (lookupProp "values" bulkDef) `shouldBe` Just "array"

  describe "Nested records" $ do
    it "decodes nested objects" $ do
      result <- callTyped "query_data"
        [("filters", object ["tags" .= (["a", "b"] :: [Text]), "maxCount" .= (7 :: Int)])]
      result `shouldBeTextResult` "Query tags=a,b max=7"

    it "treats missing optional nested fields as Nothing" $ do
      result <- callTyped "query_data"
        [("filters", object ["tags" .= (["a"] :: [Text])])]
      result `shouldBeTextResult` "Query tags=a"

    it "reports missing required nested fields with their path" $ do
      result <- callTyped "query_data" [("filters", object [])]
      expectInvalidParams result "field 'tags' is missing"

    it "rejects non-object values for record fields" $ do
      result <- callTyped "query_data" [("filters", String "nope")]
      expectInvalidParams result "field 'filters'"

    it "generates a nested object schema" $ do
      toolDefs <- fst typedHandlers anonCtx
      let queryDef = toolNamed "query_data" toolDefs
      case lookupProp "filters" queryDef of
        Just prop -> do
          schemaTypeName prop `shouldBe` "object"
          map fst (schemaProps prop) `shouldBe` ["tags", "maxCount"]
          schemaRequired prop `shouldBe` ["tags"]
        Nothing -> expectationFailure "filters property missing"

  describe "Tool execution errors" $ do
    it "reports handler failures as isError results, not protocol errors" $ do
      result <- callTyped "always_fails" [("reason", String "boom")]
      case result of
        Right tr -> do
          toolResultIsError tr `shouldBe` True
          toolResultContent tr `shouldBe` [ContentText "boom"]
        Left err -> expectationFailure $ "Expected isError result but got protocol error: " ++ show err

  describe "Multi-message prompts" $ do
    it "returns a described multi-message conversation with roles" $ do
      result <- snd convHandlers anonCtx "conversation" (promptArgsMap [("topic", "haskell")])
      case result of
        Right (PromptResult desc msgs) -> do
          desc `shouldBe` Just "About haskell"
          map promptMessageRole msgs `shouldBe` [RoleUser, RoleAssistant]
        Left err -> expectationFailure $ "Expected prompt result but got: " ++ show err
