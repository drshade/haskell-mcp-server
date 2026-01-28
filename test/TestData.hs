{-# LANGUAGE OverloadedStrings #-}

module TestData
  ( -- * Prompt Test Data
    promptTestCases
  , PromptTestCase(..)

    -- * Resource Test Data
  , resourceTestCases
  , ResourceTestCase(..)

    -- * Tool Test Data
  , toolTestCases
  , ToolTestCase(..)

    -- * Schema Test Data
  , schemaTestCases
  , SchemaTestCase(..)

    -- * Advanced Derivation Test Data
  , separateParamsTestCases
  , recursiveParamsTestCases
  , SeparateParamsTestCase(..)
  , RecursiveParamsTestCase(..)
  ) where

import Data.Text (Text)

-- | Test case for prompt derivation
data PromptTestCase = PromptTestCase
  { promptName :: Text
  , promptArgs :: [(Text, Text)]
  , expectedResult :: Text
  , testDescription :: Text
  } deriving (Show, Eq)

-- | Test case for resource derivation
data ResourceTestCase = ResourceTestCase
  { resourceUri :: Text
  , resourceExpectedContent :: Text
  , useSubstringMatch :: Bool  -- True for substring match, False for exact match
  , resourceTestDescription :: Text
  } deriving (Show, Eq)

-- | Test case for tool derivation
data ToolTestCase = ToolTestCase
  { toolName :: Text
  , toolArgs :: [(Text, Text)]
  , toolExpectedResult :: Text
  , toolTestDescription :: Text
  } deriving (Show, Eq)

-- | Test case for schema validation
data SchemaTestCase = SchemaTestCase
  { schemaToolName :: Text
  , expectedProperties :: [(Text, Text)] -- (property name, expected type)
  , requiredFields :: [Text]
  , schemaTestDescription :: Text
  } deriving (Show, Eq)

-- | Test case for separate parameter types
data SeparateParamsTestCase = SeparateParamsTestCase
  { sepToolName :: Text
  , sepArgs :: [(Text, Text)]
  , sepExpectedResult :: Text
  , sepExpectedProperties :: [Text]
  , sepTestDescription :: Text
  } deriving (Show, Eq)

-- | Test case for recursive parameter types
data RecursiveParamsTestCase = RecursiveParamsTestCase
  { recToolName :: Text
  , recArgs :: [(Text, Text)]
  , recExpectedResult :: Text
  , recExpectedProperties :: [Text]
  , recTestDescription :: Text
  } deriving (Show, Eq)

-- | Prompt test cases
promptTestCases :: [PromptTestCase]
promptTestCases =
  [ PromptTestCase
      { promptName = "simple_prompt"
      , promptArgs = [("message", "hello")]
      , expectedResult = "Simple prompt: hello"
      , testDescription = "handles simple prompts correctly"
      }
  , PromptTestCase
      { promptName = "complex_prompt"
      , promptArgs = [("title", "urgent task"), ("priority", "5"), ("urgent", "true")]
      , expectedResult = "Complex prompt: urgent task (priority=5, urgent=True)"
      , testDescription = "handles complex prompts with multiple types"
      }
  , PromptTestCase
      { promptName = "optional_prompt"
      , promptArgs = [("required", "test")]
      , expectedResult = "Optional prompt: test"
      , testDescription = "handles optional prompts with missing optional field"
      }
  , PromptTestCase
      { promptName = "optional_prompt"
      , promptArgs = [("required", "test"), ("optional", "42")]
      , expectedResult = "Optional prompt: test optional=42"
      , testDescription = "handles optional prompts with optional field present"
      }
  ]

-- | Resource test cases
resourceTestCases :: [ResourceTestCase]
resourceTestCases =
  [ ResourceTestCase
      { resourceUri = "resource://config_file"
      , resourceExpectedContent = "Config file contents"
      , useSubstringMatch = True
      , resourceTestDescription = "handles simple resources correctly"
      }
  , ResourceTestCase
      { resourceUri = "resource://database_connection"
      , resourceExpectedContent = "Database at localhost:5432"
      , useSubstringMatch = False
      , resourceTestDescription = "handles parameterized resources correctly"
      }
  ]

-- | Tool test cases
toolTestCases :: [ToolTestCase]
toolTestCases =
  [ ToolTestCase
      { toolName = "calculate"
      , toolArgs = [("operation", "add"), ("x", "10"), ("y", "5")]
      , toolExpectedResult = "15"
      , toolTestDescription = "handles simple tool calls correctly"
      }
  , ToolTestCase
      { toolName = "calculate"
      , toolArgs = [("operation", "multiply"), ("x", "7"), ("y", "6")]
      , toolExpectedResult = "42"
      , toolTestDescription = "handles tool calls with different operations"
      }
  ]

-- | Schema test cases
schemaTestCases :: [SchemaTestCase]
schemaTestCases =
  [ SchemaTestCase
      { schemaToolName = "calculate"
      , expectedProperties = [("x", "integer"), ("y", "integer"), ("operation", "string")]
      , requiredFields = ["operation", "x", "y"]
      , schemaTestDescription = "generates correct schema for Calculate tool"
      }
  , SchemaTestCase
      { schemaToolName = "echo"
      , expectedProperties = [("text", "string")]
      , requiredFields = ["text"]
      , schemaTestDescription = "generates correct schema for Echo tool"
      }
  ]

-- | Separate parameter types test cases
separateParamsTestCases :: [SeparateParamsTestCase]
separateParamsTestCases =
  [ SeparateParamsTestCase
      { sepToolName = "get_value"
      , sepArgs = [("_gvpKey", "mykey")]
      , sepExpectedResult = "Getting value for key: mykey"
      , sepExpectedProperties = ["_gvpKey"]
      , sepTestDescription = "executes GetValue with separate parameters correctly"
      }
  , SeparateParamsTestCase
      { sepToolName = "set_value"
      , sepArgs = [("_svpKey", "mykey"), ("_svpValue", "myvalue")]
      , sepExpectedResult = "Setting mykey = myvalue"
      , sepExpectedProperties = ["_svpKey", "_svpValue"]
      , sepTestDescription = "executes SetValue with separate parameters correctly"
      }
  ]

-- | Recursive parameter types test cases
recursiveParamsTestCases :: [RecursiveParamsTestCase]
recursiveParamsTestCases =
  [ RecursiveParamsTestCase
      { recToolName = "process_data"
      , recArgs = [("_ipName", "Alice"), ("_ipAge", "30")]
      , recExpectedResult = "Processing data for Alice (age 30)"
      , recExpectedProperties = ["_ipName", "_ipAge"]
      , recTestDescription = "executes recursive parameter tools correctly"
      }
  ]
