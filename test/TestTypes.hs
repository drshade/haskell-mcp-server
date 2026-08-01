{-# LANGUAGE OverloadedStrings #-}

module TestTypes where

import           Data.Text  (Text)
import qualified Data.Text  as T
import           MCP.Server (ClientContext, anonymousContext, Content (..), MessageRole (..),
                             PromptMessage (..), PromptResult (..),
                             ResourceContent (..), ToolOutput (..),
                             ToolResult, toolError,
                             toolResult)
import           Network.URI (URI)

-- Context passed to handlers in tests (no transport-level identity)
anonCtx :: ClientContext
anonCtx = anonymousContext

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
handleTestPrompt :: ClientContext -> TestPrompt -> IO Content
handleTestPrompt _ (SimplePrompt msg) =
    pure $ ContentText $ "Simple prompt: " <> msg
handleTestPrompt _ (ComplexPrompt title prio urgent) =
    pure $ ContentText $ "Complex prompt: " <> title <> " (priority=" <> T.pack (show prio) <> ", urgent=" <> T.pack (show urgent) <> ")"
handleTestPrompt _ (OptionalPrompt req opt) =
    pure $ ContentText $ "Optional prompt: " <> req <> maybe "" ((" optional=" <>) . T.pack . show) opt

handleTestResource :: ClientContext -> URI -> TestResource -> IO ResourceContent
handleTestResource _ uri ConfigFile =
    pure $ ResourceText uri "text/plain" "Config file contents: debug=true, timeout=30"
handleTestResource _ uri DatabaseConnection =
    pure $ ResourceText uri "text/plain" "Database at localhost:5432"
handleTestResource _ uri UserProfile =
    pure $ ResourceText uri "text/plain" "User profile for ID 123"

handleTestTool :: ClientContext -> TestTool -> IO Content
handleTestTool _ (Echo text) =
    pure $ ContentText $ "Echo: " <> text
handleTestTool _ (Calculate op x y) =
    let result = case op of
            "add" -> x + y
            "multiply" -> x * y
            "subtract" -> x - y
            _ -> 0
    in pure $ ContentText $ T.pack (show result)
handleTestTool _ (Toggle flag) =
    pure $ ContentText $ "Flag is now: " <> T.pack (show (not flag))
handleTestTool _ (Search query limit caseSens) =
    pure $ ContentText $ "Search results for '" <> query <> "'" <>
        maybe "" ((" (limit=" <>) . (<> ")") . T.pack . show) limit <>
        maybe "" ((" (case-sensitive=" <>) . (<> ")") . T.pack . show) caseSens

-- Handler for separate params tool
handleSeparateParamsTool :: ClientContext -> SeparateParamsTool -> IO Content
handleSeparateParamsTool _ (GetValue (GetValueParams key)) =
    pure $ ContentText $ "Getting value for key: " <> key
handleSeparateParamsTool _ (SetValue (SetValueParams key value)) =
    pure $ ContentText $ "Setting " <> key <> " = " <> value

-- Handler for recursive tool
handleRecursiveTool :: ClientContext -> RecursiveTool -> IO Content
handleRecursiveTool _ (ProcessData (MiddleParams (InnerParams name age))) =
    pure $ ContentText $ "Processing data for " <> name <> " (age " <> T.pack (show age) <> ")"

-- Type covering all parseable field types for exhaustive parsing tests
data AllTypesTool
    = RequiredFields
        { rfText :: Text
        , rfInt :: Int
        , rfInteger :: Integer
        , rfDouble :: Double
        , rfFloat :: Float
        , rfBool :: Bool
        }
    | OptionalFields
        { ofText :: Maybe Text
        , ofInt :: Maybe Int
        , ofInteger :: Maybe Integer
        , ofDouble :: Maybe Double
        , ofFloat :: Maybe Float
        , ofBool :: Maybe Bool
        }
    deriving (Show, Eq)

handleAllTypesTool :: ClientContext -> AllTypesTool -> IO Content
handleAllTypesTool _ (RequiredFields t i ig d f b) =
    pure $ ContentText $ T.intercalate ", "
        [ "text=" <> t
        , "int=" <> T.pack (show i)
        , "integer=" <> T.pack (show ig)
        , "double=" <> T.pack (show d)
        , "float=" <> T.pack (show f)
        , "bool=" <> T.pack (show b)
        ]
handleAllTypesTool _ (OptionalFields t i ig d f b) =
    pure $ ContentText $ T.intercalate ", "
        [ "text=" <> maybe "Nothing" id t
        , "int=" <> maybe "Nothing" (T.pack . show) i
        , "integer=" <> maybe "Nothing" (T.pack . show) ig
        , "double=" <> maybe "Nothing" (T.pack . show) d
        , "float=" <> maybe "Nothing" (T.pack . show) f
        , "bool=" <> maybe "Nothing" (T.pack . show) b
        ]

-- Types exercising 0.2 typed arguments: enums, lists, nested records
data Color = Red | Green | Blue
    deriving (Show, Eq)

data Filters = Filters { tags :: [Text], maxCount :: Maybe Int }
    deriving (Show, Eq)

data TypedTool
    = Paint { color :: Color, brightness :: Int }
    | BulkAdd { values :: [Int] }
    | QueryData { filters :: Filters }
    | AlwaysFails { reason :: Text }
    deriving (Show, Eq)

handleTypedTool :: ClientContext -> TypedTool -> IO ToolResult
handleTypedTool _ (Paint c b) =
    pure $ toolResult [ContentText $ "Painting " <> T.pack (show c) <> " at " <> T.pack (show b)]
handleTypedTool _ (BulkAdd vs) =
    pure $ toolResult [ContentText $ "Sum: " <> T.pack (show (sum vs))]
handleTypedTool _ (QueryData (Filters ts mc)) =
    pure $ toolResult [ContentText $ "Query tags=" <> T.intercalate "," ts
                        <> maybe "" ((" max=" <>) . T.pack . show) mc]
handleTypedTool _ (AlwaysFails msg) =
    pure $ toolError msg

-- Resource type mixing static resources and templates (record constructors)
data TemplResource
    = Catalog
    | MemberProfile { memberId :: Text }
    | OrderItem { orderId :: Int, itemName :: Text }
    deriving (Show, Eq)

handleTemplResource :: ClientContext -> URI -> TemplResource -> IO ResourceContent
handleTemplResource _ uri Catalog =
    pure $ ResourceText uri "text/plain" "The catalog"
handleTemplResource _ uri (MemberProfile uid) =
    pure $ ResourceText uri "text/plain" ("Profile of " <> uid)
handleTemplResource _ uri (OrderItem oid item) =
    pure $ ResourceText uri "text/plain" ("Order " <> T.pack (show oid) <> " item " <> item)

-- Types exercising derived structured output (ADR 0005)
data Wind = Wind { windSpeedKph :: Double, windGusting :: Bool }
    deriving (Show, Eq)

data Sky = SkyClear | SkyCloudy | SkyRaining
    deriving (Show, Eq)

data WeatherReport = WeatherReport
    { wrTemperature :: Int
    , wrSky         :: Sky
    , wrWind        :: Wind
    , wrAlerts      :: [Text]
    , wrHumidity    :: Maybe Int
    } deriving (Show, Eq)

data WeatherTool
    = GetWeather { wtCity :: Text }
    | BrokenStation { wtStation :: Text }
    | CustomReport { wtRegion :: Text }
    | PlainForecast { wtNote :: Text }
    deriving (Show, Eq)

sampleReport :: Text -> Maybe Int -> WeatherReport
sampleReport place humidity = WeatherReport
    { wrTemperature = 21
    , wrSky = SkyCloudy
    , wrWind = Wind 12.5 False
    , wrAlerts = ["wind advisory for " <> place]
    , wrHumidity = humidity
    }

handleWeatherTool :: ClientContext -> WeatherTool -> IO (ToolOutput WeatherReport)
handleWeatherTool _ (GetWeather c) =
    pure $ ToolOutput (sampleReport c (Just 60))
handleWeatherTool _ (BrokenStation s) =
    pure $ ToolOutputError ("station offline: " <> s)
handleWeatherTool _ (CustomReport r) =
    pure $ ToolOutputWith [ContentText ("Report for " <> r)] (sampleReport r Nothing)
handleWeatherTool _ (PlainForecast n) =
    pure $ ToolOutputRaw (toolResult [ContentText n])

-- A prompt whose handler produces a multi-message conversation
data ConvPrompt = Conversation { topic :: Text }
    deriving (Show, Eq)

handleConvPrompt :: ClientContext -> ConvPrompt -> IO PromptResult
handleConvPrompt _ (Conversation t) = pure $ PromptResult
    { promptResultDescription = Just ("About " <> t)
    , promptResultMessages =
        [ PromptMessage RoleUser (ContentText ("Tell me about " <> t))
        , PromptMessage RoleAssistant (ContentText ("Here is what I know about " <> t))
        ]
    }

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
