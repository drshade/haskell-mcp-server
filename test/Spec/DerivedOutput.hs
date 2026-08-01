{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Coverage for derived structured output (ADR 0005): the outputSchema
-- generated from a result record, and the ToolOutput result forms.
module Spec.DerivedOutput (spec) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import MCP.Server
import MCP.Server.Derive
import Test.Hspec
import TestHelpers
import TestTypes

weatherHandlers :: (ToolListHandler, ToolCallHandler)
weatherHandlers = $(deriveToolHandlerWithOutputDescription ''WeatherTool 'handleWeatherTool ''WeatherReport
  [ ("GetWeather", "Current weather"), ("wrTemperature", "Degrees Celsius") ])

callWeather :: Text -> [(Text, Value)] -> IO (Either Error ToolResult)
callWeather name args = snd weatherHandlers anonCtx name (valueArgs args)

-- The serialized form the derivation guarantees for 'sampleReport'
expectedReport :: Text -> Maybe Int -> Value
expectedReport place humidity = object $
  [ "wrTemperature" .= (21 :: Int)
  , "wrSky" .= ("sky_cloudy" :: Text)
  , "wrWind" .= object ["windSpeedKph" .= (12.5 :: Double), "windGusting" .= False]
  , "wrAlerts" .= (["wind advisory for " <> place] :: [Text])
  ] ++ maybe [] (\h -> ["wrHumidity" .= h]) humidity

spec :: Spec
spec = describe "Derived structured output" $ do

  describe "outputSchema derivation" $ do
    it "every tool of the splice carries the output record's object schema" $ do
      defs <- fst weatherHandlers anonCtx
      map toolDefinitionName defs `shouldBe`
        ["get_weather", "broken_station", "custom_report", "plain_forecast"]
      case map toolDefinitionOutputSchema defs of
        (s:rest) -> do
          s `shouldSatisfy` (/= Nothing)
          all (== s) rest `shouldBe` True
        [] -> expectationFailure "no tools derived"

    it "mirrors field types: enum, nested record, list, optional Maybe" $ do
      defs <- fst weatherHandlers anonCtx
      case defs of
        [] -> expectationFailure "no tools derived"
        (d:_) -> case toolDefinitionOutputSchema d of
          Nothing -> expectationFailure "outputSchema missing"
          Just s -> do
            schemaRequired s `shouldBe` ["wrTemperature", "wrSky", "wrWind", "wrAlerts"]
            fmap schemaTypeName (lookup "wrSky" (schemaProps s)) `shouldBe` Just "string"
            fmap schemaShape (lookup "wrSky" (schemaProps s)) `shouldBe`
              Just (SchemaString (Just ["sky_clear", "sky_cloudy", "sky_raining"]))
            fmap schemaTypeName (lookup "wrWind" (schemaProps s)) `shouldBe` Just "object"
            fmap schemaTypeName (lookup "wrAlerts" (schemaProps s)) `shouldBe` Just "array"
            fmap schemaDescription (lookup "wrTemperature" (schemaProps s)) `shouldBe`
              Just (Just "Degrees Celsius")

  describe "ToolOutput results" $ do
    it "serializes the typed value into structuredContent with a matching text block" $ do
      result <- callWeather "get_weather" [("wtCity", String "Cape Town")]
      case result of
        Right tr -> do
          toolResultIsError tr `shouldBe` False
          toolResultStructured tr `shouldBe` Just (expectedReport "Cape Town" (Just 60))
          case toolResultContent tr of
            [ContentText t] ->
              decode (BSL.fromStrict (TE.encodeUtf8 t)) `shouldBe`
                Just (expectedReport "Cape Town" (Just 60))
            other -> expectationFailure $ "expected one text block, got: " ++ show other
        Left err -> expectationFailure $ "expected result, got error: " ++ show err

    it "omits Nothing fields from the serialized value" $ do
      result <- callWeather "custom_report" [("wtRegion", String "Karoo")]
      case result of
        Right tr -> toolResultStructured tr `shouldBe` Just (expectedReport "Karoo" Nothing)
        Left err -> expectationFailure $ "expected result, got error: " ++ show err

    it "keeps caller-supplied content blocks with ToolOutputWith" $ do
      result <- callWeather "custom_report" [("wtRegion", String "Karoo")]
      case result of
        Right tr -> toolResultContent tr `shouldBe` [ContentText "Report for Karoo"]
        Left err -> expectationFailure $ "expected result, got error: " ++ show err

    it "reports ToolOutputError as an isError result" $ do
      result <- callWeather "broken_station" [("wtStation", String "X1")]
      case result of
        Right tr -> do
          toolResultIsError tr `shouldBe` True
          toolResultStructured tr `shouldBe` Nothing
          toolResultContent tr `shouldBe` [ContentText "station offline: X1"]
        Left err -> expectationFailure $ "expected isError result, got: " ++ show err

    it "passes ToolOutputRaw through untouched" $ do
      result <- callWeather "plain_forecast" [("wtNote", String "sunny")]
      case result of
        Right tr -> do
          toolResultStructured tr `shouldBe` Nothing
          toolResultContent tr `shouldBe` [ContentText "sunny"]
        Left err -> expectationFailure $ "expected result, got error: " ++ show err
