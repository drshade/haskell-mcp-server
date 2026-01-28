{-# LANGUAGE OverloadedStrings #-}

module Spec.JSONConversion (spec) where

import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server
import Test.Hspec
import Test.QuickCheck
import Text.Read (readMaybe)

-- Arbitrary instance for Text (needed for QuickCheck)
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

spec :: Spec
spec = describe "JSON Type Conversion" $ do
  describe "Property-based tests" $ do
    it "round-trips integers correctly" $ property prop_intRoundTrip
    it "round-trips booleans correctly" $ property prop_boolRoundTrip
    it "round-trips text correctly" $ property prop_textRoundTrip

  describe "Manual conversion tests" $ do
    it "converts integers manually" testIntConversion
    it "converts booleans manually" testBoolConversion

-- Fixed property-based tests (no more 'error' usage)
prop_intRoundTrip :: Int -> Bool
prop_intRoundTrip i =
    let jsonVal = toJSON i
        textVal = jsonValueToText jsonVal
        parsed = readMaybe (T.unpack textVal)
    in parsed == Just i

prop_boolRoundTrip :: Bool -> Bool
prop_boolRoundTrip b =
    let jsonVal = toJSON b
        textVal = jsonValueToText jsonVal
        parsed = case T.toLower textVal of
                   "true" -> Just True
                   "false" -> Just False
                   _ -> Nothing
    in parsed == Just b

prop_textRoundTrip :: Text -> Bool
prop_textRoundTrip t =
    let jsonVal = toJSON t
        textVal = jsonValueToText jsonVal
    in textVal == t

-- Manual test functions converted to Hspec style
testIntConversion :: IO ()
testIntConversion = do
    let testCases = [0, 42, -17, 999999] :: [Int]
    results <- mapM testSingleInt testCases
    all id results `shouldBe` True
  where
    testSingleInt i = do
        let jsonVal = toJSON i
            textVal = jsonValueToText jsonVal
        case readMaybe (T.unpack textVal) of
            Just parsed -> return (parsed == i)
            Nothing -> do
                expectationFailure $ "Failed to parse Int from: " <> T.unpack textVal
                return False

testBoolConversion :: IO ()
testBoolConversion = do
    let testCases = [True, False]
    results <- mapM testSingleBool testCases
    all id results `shouldBe` True
  where
    testSingleBool b = do
        let jsonVal = toJSON b
            textVal = jsonValueToText jsonVal
            expected = if b then "true" else "false"
        case T.toLower textVal of
            result | result == expected -> return True
            result -> do
                expectationFailure $ "Expected " <> T.unpack expected <> " but got " <> T.unpack result
                return False
