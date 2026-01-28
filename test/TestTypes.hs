{-# LANGUAGE OverloadedStrings #-}

module TestTypes where

import           Data.Text  (Text)
import qualified Data.Text  as T
import           MCP.Server (Content(..), ResourceContent(..))
import           Network.URI (URI)

-- Test data types for end-to-end testing
data TestPrompt
    = SimplePrompt { message :: Text }
    | ComplexPrompt { title :: Text, priority :: Int, urgent :: Bool }
    | OptionalPrompt { required :: Text, optional :: Maybe Int }
    deriving (Show, Eq)

data TestResource
    = ConfigFile
    | DatabaseConnection
    | UserProfile
    deriving (Show, Eq)

data TestTool
    = Echo { text :: Text }
    | Calculate { operation :: Text, x :: Int, y :: Int }
    | Toggle { flag :: Bool }
    | Search { query :: Text, limit :: Maybe Int, caseSensitive :: Maybe Bool }
    deriving (Show, Eq)

-- Test separate parameter types approach (should fail with current implementation)
data GetValueParams = GetValueParams { _gvpKey :: Text }
    deriving (Show, Eq)
data SetValueParams = SetValueParams { _svpKey :: Text, _svpValue :: Text }
    deriving (Show, Eq)

data SeparateParamsTool
    = GetValue GetValueParams
    | SetValue SetValueParams
    deriving (Show, Eq)

-- Test recursive parameter types
data InnerParams = InnerParams { _ipName :: Text, _ipAge :: Int }
    deriving (Show, Eq)
data MiddleParams = MiddleParams InnerParams
    deriving (Show, Eq)
data RecursiveTool = ProcessData MiddleParams
    deriving (Show, Eq)

-- Handler functions
handleTestPrompt :: TestPrompt -> IO Content
handleTestPrompt (SimplePrompt msg) =
    pure $ ContentText $ "Simple prompt: " <> msg
handleTestPrompt (ComplexPrompt title prio urgent) =
    pure $ ContentText $ "Complex prompt: " <> title <> " (priority=" <> T.pack (show prio) <> ", urgent=" <> T.pack (show urgent) <> ")"
handleTestPrompt (OptionalPrompt req opt) =
    pure $ ContentText $ "Optional prompt: " <> req <> maybe "" ((" optional=" <>) . T.pack . show) opt

handleTestResource :: URI -> TestResource -> IO ResourceContent
handleTestResource uri ConfigFile =
    pure $ ResourceText uri "text/plain" "Config file contents: debug=true, timeout=30"
handleTestResource uri DatabaseConnection =
    pure $ ResourceText uri "text/plain" "Database at localhost:5432"
handleTestResource uri UserProfile =
    pure $ ResourceText uri "text/plain" "User profile for ID 123"

handleTestTool :: TestTool -> IO Content
handleTestTool (Echo text) =
    pure $ ContentText $ "Echo: " <> text
handleTestTool (Calculate op x y) =
    let result = case op of
            "add" -> x + y
            "multiply" -> x * y
            "subtract" -> x - y
            _ -> 0
    in pure $ ContentText $ T.pack (show result)
handleTestTool (Toggle flag) =
    pure $ ContentText $ "Flag is now: " <> T.pack (show (not flag))
handleTestTool (Search query limit caseSens) =
    pure $ ContentText $ "Search results for '" <> query <> "'" <>
        maybe "" ((" (limit=" <>) . (<> ")") . T.pack . show) limit <>
        maybe "" ((" (case-sensitive=" <>) . (<> ")") . T.pack . show) caseSens

-- Handler for separate params tool
handleSeparateParamsTool :: SeparateParamsTool -> IO Content
handleSeparateParamsTool (GetValue (GetValueParams key)) =
    pure $ ContentText $ "Getting value for key: " <> key
handleSeparateParamsTool (SetValue (SetValueParams key value)) =
    pure $ ContentText $ "Setting " <> key <> " = " <> value

-- Handler for recursive tool
handleRecursiveTool :: RecursiveTool -> IO Content
handleRecursiveTool (ProcessData (MiddleParams (InnerParams name age))) =
    pure $ ContentText $ "Processing data for " <> name <> " (age " <> T.pack (show age) <> ")"

-- Test descriptions for custom description functionality
testDescriptions :: [(String, String)]
testDescriptions =
    [ ("Echo", "Echoes the input text back to the user")
    , ("Calculate", "Performs mathematical calculations")
    , ("text", "The text to echo back")
    , ("operation", "The mathematical operation to perform")
    , ("x", "The first number")
    , ("y", "The second number")
    ]

-- Test descriptions for separate parameter types
separateParamsDescriptions :: [(String, String)]
separateParamsDescriptions =
    [ ("GetValue", "Retrieves a value from the key-value store")
    , ("SetValue", "Sets a value in the key-value store")
    , ("_gvpKey", "The key to retrieve the value for")
    , ("_svpKey", "The key to set the value for")
    , ("_svpValue", "The value to store")
    , ("ProcessData", "Processes user data with age validation")
    , ("_ipName", "The person's full name")
    , ("_ipAge", "The person's age in years")
    ]
