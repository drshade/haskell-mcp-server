{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Spec.BasicDerivation (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server
import MCP.Server.Derive
import Test.Hspec
import TestTypes
import TestData

-- Generate handlers using Template Haskell
testPromptHandlers :: (PromptListHandler IO, PromptGetHandler IO)
testPromptHandlers = $(derivePromptHandler ''TestPrompt 'handleTestPrompt)

testResourceHandlers :: (ResourceListHandler IO, ResourceReadHandler IO)
testResourceHandlers = $(deriveResourceHandler ''TestResource 'handleTestResource)

testToolHandlers :: (ToolListHandler IO, ToolCallHandler IO)
testToolHandlers = $(deriveToolHandler ''TestTool 'handleTestTool)

-- Helper functions for common test patterns
shouldReturnContentText :: IO (Either Error Content) -> Text -> IO ()
shouldReturnContentText action expected = do
  result <- action
  case result of
    Right (ContentText content) -> content `shouldBe` expected
    other -> expectationFailure $ "Expected ContentText '" ++ T.unpack expected ++ "' but got: " ++ show other

shouldContainText :: IO (Either Error Content) -> Text -> IO ()
shouldContainText action expectedSubstring = do
  result <- action
  case result of
    Right (ContentText content) ->
      T.isInfixOf expectedSubstring content `shouldBe` True
    other -> expectationFailure $ "Expected ContentText containing '" ++ T.unpack expectedSubstring ++ "' but got: " ++ show other

testPromptCall :: PromptGetHandler IO -> Text -> [(Text, Text)] -> Text -> IO ()
testPromptCall handler name args expected =
  shouldReturnContentText (handler name args) expected

testResourceCall :: ResourceReadHandler IO -> String -> Text -> IO ()
testResourceCall handler uriString expected = do
  case parseURI uriString of
    Just uri -> do
      result <- handler uri
      case result of
        Right (ResourceText _ _ content) -> content `shouldBe` expected
        Right (ResourceBlob _ _ _) -> expectationFailure "Expected ResourceText but got ResourceBlob"
        Left err -> expectationFailure $ "Expected success but got error: " ++ show err
    Nothing -> expectationFailure $ "Failed to parse URI: " ++ uriString

testToolCall :: ToolCallHandler IO -> Text -> [(Text, Text)] -> Text -> IO ()
testToolCall handler name args expected =
  shouldReturnContentText (handler name args) expected

spec :: Spec
spec = describe "Basic Template Haskell Derivation" $ do

  describe "Prompt derivation" $ do
    let (_, getHandler) = testPromptHandlers

    forM_ promptTestCases $ \testCase ->
      it (T.unpack $ testDescription testCase) $
        testPromptCall getHandler (promptName testCase) (promptArgs testCase) (expectedResult testCase)

  describe "Resource derivation" $ do
    let (_, readHandler) = testResourceHandlers

    forM_ resourceTestCases $ \testCase ->
      it (T.unpack $ resourceTestDescription testCase) $ do
        case parseURI (T.unpack $ TestData.resourceUri testCase) of
          Just uri ->
            if useSubstringMatch testCase
              then do
                result <- readHandler uri
                case result of
                  Right (ResourceText _ _ content) ->
                    T.isInfixOf (resourceExpectedContent testCase) content `shouldBe` True
                  Right (ResourceBlob _ _ _) -> expectationFailure "Expected ResourceText but got ResourceBlob"
                  Left err -> expectationFailure $ "Expected success but got error: " ++ show err
              else testResourceCall readHandler (T.unpack $ TestData.resourceUri testCase) (resourceExpectedContent testCase)
          Nothing -> expectationFailure $ "Failed to parse URI: " ++ T.unpack (TestData.resourceUri testCase)

  describe "Tool derivation" $ do
    let (_, callHandler) = testToolHandlers

    forM_ toolTestCases $ \testCase ->
      it (T.unpack $ toolTestDescription testCase) $
        testToolCall callHandler (toolName testCase) (toolArgs testCase) (toolExpectedResult testCase)
