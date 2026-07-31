{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Spec.ToolCallParsing (spec) where

import Data.Aeson (Value (..))
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server
import MCP.Server.Derive
import Test.Hspec
import TestHelpers
import TestTypes

allTypesHandlers :: (ToolListHandler, ToolCallHandler)
allTypesHandlers = $(deriveToolHandler ''AllTypesTool 'handleAllTypesTool)

callTool :: Text -> [(Text, Text)] -> IO (Either Error ToolResult)
callTool name args = snd allTypesHandlers anonCtx name (stringArgs args)

shouldBeRight :: IO (Either Error ToolResult) -> Text -> IO ()
shouldBeRight action expected = do
  result <- action
  result `shouldBeTextResult` expected

shouldBeInvalidParams :: IO (Either Error ToolResult) -> Text -> IO ()
shouldBeInvalidParams action expectedSubstring = do
  result <- action
  case result of
    Left (InvalidParams msg) ->
      T.isInfixOf expectedSubstring msg `shouldBe` True
    other -> expectationFailure $ "Expected InvalidParams containing '" ++ T.unpack expectedSubstring ++ "' but got: " ++ show other

shouldBeMissingParams :: IO (Either Error ToolResult) -> Text -> IO ()
shouldBeMissingParams action expectedSubstring = do
  result <- action
  case result of
    Left (MissingRequiredParams msg) ->
      T.isInfixOf expectedSubstring msg `shouldBe` True
    other -> expectationFailure $ "Expected MissingRequiredParams containing '" ++ T.unpack expectedSubstring ++ "' but got: " ++ show other

spec :: Spec
spec = describe "Tool call parsing" $ do

  describe "Required fields — successful parse" $ do
    it "parses all required field types" $
      shouldBeRight
        (callTool "required_fields"
          [ ("rfText", "hello")
          , ("rfInt", "42")
          , ("rfInteger", "100")
          , ("rfDouble", "3.14")
          , ("rfFloat", "2.5")
          , ("rfBool", "true")
          ])
        "text=hello, int=42, integer=100, double=3.14, float=2.5, bool=True"

  describe "Optional fields — present, successful parse" $ do
    it "parses all optional field types when present" $
      shouldBeRight
        (callTool "optional_fields"
          [ ("ofText", "world")
          , ("ofInt", "7")
          , ("ofInteger", "999")
          , ("ofDouble", "1.5")
          , ("ofFloat", "0.5")
          , ("ofBool", "false")
          ])
        "text=world, int=7, integer=999, double=1.5, float=0.5, bool=False"

  describe "Optional fields — missing" $ do
    it "handles all optional fields omitted" $
      shouldBeRight
        (callTool "optional_fields" [])
        "text=Nothing, int=Nothing, integer=Nothing, double=Nothing, float=Nothing, bool=Nothing"

  describe "Required fields — parse failure" $ do
    it "fails to parse Int from non-numeric string" $
      shouldBeInvalidParams
        (callTool "required_fields"
          [ ("rfText", "hello"), ("rfInt", "not_a_number"), ("rfInteger", "1")
          , ("rfDouble", "1.0"), ("rfFloat", "1.0"), ("rfBool", "true")
          ])
        "field 'rfInt': Failed to parse Int from: not_a_number"

    it "fails to parse Integer from non-numeric string" $
      shouldBeInvalidParams
        (callTool "required_fields"
          [ ("rfText", "hello"), ("rfInt", "1"), ("rfInteger", "nope")
          , ("rfDouble", "1.0"), ("rfFloat", "1.0"), ("rfBool", "true")
          ])
        "field 'rfInteger': Failed to parse Integer from: nope"

    it "fails to parse Double from non-numeric string" $
      shouldBeInvalidParams
        (callTool "required_fields"
          [ ("rfText", "hello"), ("rfInt", "1"), ("rfInteger", "1")
          , ("rfDouble", "abc"), ("rfFloat", "1.0"), ("rfBool", "true")
          ])
        "field 'rfDouble': Failed to parse Double from: abc"

    it "fails to parse Float from non-numeric string" $
      shouldBeInvalidParams
        (callTool "required_fields"
          [ ("rfText", "hello"), ("rfInt", "1"), ("rfInteger", "1")
          , ("rfDouble", "1.0"), ("rfFloat", "xyz"), ("rfBool", "true")
          ])
        "field 'rfFloat': Failed to parse Float from: xyz"

    it "fails to parse Bool from invalid string" $
      shouldBeInvalidParams
        (callTool "required_fields"
          [ ("rfText", "hello"), ("rfInt", "1"), ("rfInteger", "1")
          , ("rfDouble", "1.0"), ("rfFloat", "1.0"), ("rfBool", "maybe")
          ])
        "field 'rfBool': Failed to parse Bool from: maybe"

  describe "Optional fields — parse failure" $ do
    it "fails to parse optional Int from invalid value" $
      shouldBeInvalidParams
        (callTool "optional_fields" [("ofInt", "bad")])
        "field 'ofInt': Failed to parse Int from: bad"

    it "fails to parse optional Integer from invalid value" $
      shouldBeInvalidParams
        (callTool "optional_fields" [("ofInteger", "bad")])
        "field 'ofInteger': Failed to parse Integer from: bad"

    it "fails to parse optional Double from invalid value" $
      shouldBeInvalidParams
        (callTool "optional_fields" [("ofDouble", "bad")])
        "field 'ofDouble': Failed to parse Double from: bad"

    it "fails to parse optional Float from invalid value" $
      shouldBeInvalidParams
        (callTool "optional_fields" [("ofFloat", "bad")])
        "field 'ofFloat': Failed to parse Float from: bad"

    it "fails to parse optional Bool from invalid value" $
      shouldBeInvalidParams
        (callTool "optional_fields" [("ofBool", "bad")])
        "field 'ofBool': Failed to parse Bool from: bad"

  describe "Missing required field" $ do
    it "reports missing required field" $
      shouldBeMissingParams
        (callTool "required_fields" [("rfText", "hello")])
        "is missing"

  describe "Integer exponent bound" $ do
    -- Regression: aeson keeps huge exponents compact, so without a bound a
    -- ~13-byte payload like 1e100000 would materialize a 100001-digit Integer.
    it "rejects huge exponents without materializing the Integer" $ do
      result <- snd allTypesHandlers anonCtx "optional_fields"
        (valueArgs [("ofInteger", Number (read "1e100000"))])
      case result of
        Left (InvalidParams msg) ->
          msg `shouldSatisfy` T.isInfixOf "exponent too large"
        other -> expectationFailure $ "Expected InvalidParams but got: " ++ show other
