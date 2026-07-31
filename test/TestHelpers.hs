{-# LANGUAGE OverloadedStrings #-}

-- | Shared helpers for exercising the 0.2 handler API from tests.
module TestHelpers
  ( -- * Building argument maps
    stringArgs
  , valueArgs
  , promptArgsMap
    -- * Result expectations
  , shouldBeTextResult
  , shouldBeTextResultContaining
  , shouldBePromptText
    -- * Schema access
  , schemaTypeName
  , schemaProps
  , schemaRequired
  , lookupProp
  ) where

import           Data.Aeson (Value (..))
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Text  (Text)
import qualified Data.Text  as T
import           MCP.Server
import           Test.Hspec

-- | Tool arguments where every value is a JSON string (exercises the lenient
-- string-fallback parsing that pre-0.2 clients rely on).
stringArgs :: [(Text, Text)] -> Map Text Value
stringArgs = Map.fromList . map (fmap String)

-- | Tool arguments with native JSON values.
valueArgs :: [(Text, Value)] -> Map Text Value
valueArgs = Map.fromList

-- | Prompt arguments (string-valued per the MCP spec).
promptArgsMap :: [(Text, Text)] -> Map Text Text
promptArgsMap = Map.fromList

-- | Expect a successful, non-error tool result with exactly one text block.
shouldBeTextResult :: Either Error ToolResult -> Text -> Expectation
shouldBeTextResult (Right tr) expected
  | not (toolResultIsError tr)
  , [ContentText content] <- toolResultContent tr = content `shouldBe` expected
shouldBeTextResult other expected =
  expectationFailure $ "Expected text result '" ++ T.unpack expected ++ "' but got: " ++ show other

-- | Like 'shouldBeTextResult' but with substring matching.
shouldBeTextResultContaining :: Either Error ToolResult -> Text -> Expectation
shouldBeTextResultContaining (Right tr) expected
  | not (toolResultIsError tr)
  , [ContentText content] <- toolResultContent tr =
      content `shouldSatisfy` T.isInfixOf expected
shouldBeTextResultContaining other expected =
  expectationFailure $ "Expected text result containing '" ++ T.unpack expected ++ "' but got: " ++ show other

-- | Expect a prompt result of a single user message with the given text.
shouldBePromptText :: Either Error PromptResult -> Text -> Expectation
shouldBePromptText (Right (PromptResult _ [PromptMessage RoleUser (ContentText content)])) expected =
  content `shouldBe` expected
shouldBePromptText other expected =
  expectationFailure $ "Expected prompt text '" ++ T.unpack expected ++ "' but got: " ++ show other

-- | The JSON Schema type keyword for a schema.
schemaTypeName :: Schema -> Text
schemaTypeName s = case schemaShape s of
  SchemaString _   -> "string"
  SchemaInteger    -> "integer"
  SchemaNumber     -> "number"
  SchemaBoolean    -> "boolean"
  SchemaArray _    -> "array"
  SchemaObject _ _ -> "object"

-- | The properties of an object schema (empty for non-objects).
schemaProps :: Schema -> [(Text, Schema)]
schemaProps s = case schemaShape s of
  SchemaObject props _ -> props
  _                    -> []

-- | The required field names of an object schema.
schemaRequired :: Schema -> [Text]
schemaRequired s = case schemaShape s of
  SchemaObject _ req -> req
  _                  -> []

-- | Look up a property schema in a tool's input schema.
lookupProp :: Text -> ToolDefinition -> Maybe Schema
lookupProp name def = lookup name (schemaProps (toolDefinitionInputSchema def))
