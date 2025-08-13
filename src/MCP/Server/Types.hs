{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.Types
  ( -- * Content Types
    Content(..)
  , ContentImageData(..)
  , ContentResourceData(..)
  , ResourceContent(..)

    -- * URI Utilities  
  , parseURI
  , URI

    -- * Error Types
  , Error(..)

    -- * Definition Types
  , PromptDefinition(..)
  , ResourceDefinition(..)
  , ToolDefinition(..)
  , ArgumentDefinition(..)
  , InputSchemaDefinition(..)
  , InputSchemaDefinitionProperty(..)

    -- * Server Types
  , McpServerInfo(..)
  , McpServerHandlers(..)
  , ServerCapabilities(..)
  , PromptCapabilities(..)
  , ResourceCapabilities(..)
  , ToolCapabilities(..)
  , LoggingCapabilities(..)

    -- * Request/Response Types
  , PromptListHandler
  , PromptGetHandler
  , ResourceListHandler
  , ResourceReadHandler
  , ToolListHandler
  , ToolCallHandler

    -- * Basic Types
  , PromptName
  , ToolName
  , ArgumentName
  , ArgumentValue
  ) where

import           Data.Aeson
import           Data.Aeson.Key   (fromText)
import           Data.Aeson.Types (Parser)
import           Data.Maybe       (catMaybes)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           GHC.Generics     (Generic)
import           Network.URI      (URI, parseURI)

type PromptName = Text
type ToolName = Text
type ArgumentName = Text
type ArgumentValue = Text

-- | Content that can be returned by prompts, resources, and tools
data Content
  = ContentText Text
  | ContentImage ContentImageData
  | ContentResource ContentResourceData
  deriving (Show, Eq, Generic)

instance ToJSON Content where
  toJSON (ContentText text) = object
    [ "type" .= ("text" :: Text)
    , "text" .= text
    ]
  toJSON (ContentImage img) = object
    [ "type" .= ("image" :: Text)
    , "data" .= contentImageData img
    , "mimeType" .= contentImageMimeType img
    ]
  toJSON (ContentResource res) = object
    [ "type" .= ("resource" :: Text)
    , "resource" .= object
        [ "uri" .= contentResourceUri res
        , "mimeType" .= contentResourceMimeType res
        ]
    ]

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    contentType <- o .: "type" :: Parser Text
    case contentType of
      "text" -> ContentText <$> o .: "text"
      "image" -> do
        imgData <- o .: "data"
        mimeType <- o .: "mimeType"
        return $ ContentImage $ ContentImageData imgData mimeType
      "resource" -> do
        res <- o .: "resource"
        uri <- res .: "uri"
        mimeType <- res .:? "mimeType"
        return $ ContentResource $ ContentResourceData uri mimeType
      _ -> fail $ "Unknown content type: " ++ T.unpack contentType

data ContentImageData = ContentImageData
  { contentImageData     :: Text
  , contentImageMimeType :: Text
  } deriving (Show, Eq, Generic)

data ContentResourceData = ContentResourceData
  { contentResourceUri      :: URI
  , contentResourceMimeType :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Resource content compliant with MCP specification
-- Must include uri and mimeType, with either text or blob data
data ResourceContent
  = ResourceText
      { resourceUri :: URI
      , resourceMimeType :: Text
      , resourceText :: Text
      }
  | ResourceBlob
      { resourceUri :: URI
      , resourceMimeType :: Text
      , resourceBlob :: Text  -- base64 encoded
      }
  deriving (Show, Eq, Generic)

instance ToJSON ResourceContent where
  toJSON (ResourceText uri mimeType text) = object
    [ "uri" .= show uri
    , "mimeType" .= mimeType
    , "text" .= text
    ]
  toJSON (ResourceBlob uri mimeType blob) = object
    [ "uri" .= show uri
    , "mimeType" .= mimeType
    , "blob" .= blob
    ]

instance FromJSON ResourceContent where
  parseJSON = withObject "ResourceContent" $ \o -> do
    uriText <- o .: "uri"
    mimeType <- o .: "mimeType"
    case parseURI uriText of
      Nothing -> fail "Invalid URI"
      Just uri -> do
        maybeText <- o .:? "text"
        maybeBlob <- o .:? "blob"
        case (maybeText, maybeBlob) of
          (Just text, Nothing) -> return $ ResourceText uri mimeType text
          (Nothing, Just blob) -> return $ ResourceBlob uri mimeType blob
          _ -> fail "ResourceContent must have either 'text' or 'blob' field"

-- | MCP protocol errors
data Error
  = InvalidPromptName Text
  | MissingRequiredParams Text
  | ResourceNotFound Text
  | InternalError Text
  | UnknownTool Text
  | InvalidRequest Text
  | MethodNotFound Text
  | InvalidParams Text
  deriving (Show, Eq, Generic)

instance ToJSON Error where
  toJSON err = object
    [ "code" .= errorCode err
    , "message" .= errorMessage err
    ]
    where
      errorCode :: Error -> Int
      errorCode (InvalidPromptName _)     = -32602
      errorCode (MissingRequiredParams _) = -32602
      errorCode (ResourceNotFound _)      = -32602
      errorCode (InternalError _)         = -32603
      errorCode (UnknownTool _)           = -32602
      errorCode (InvalidRequest _)        = -32600
      errorCode (MethodNotFound _)        = -32601
      errorCode (InvalidParams _)         = -32602

      errorMessage :: Error -> Text
      errorMessage (InvalidPromptName msg) = "Invalid prompt name: " <> msg
      errorMessage (MissingRequiredParams msg) = "Missing required parameters: " <> msg
      errorMessage (ResourceNotFound msg) = "Resource not found: " <> msg
      errorMessage (InternalError msg) = "Internal error: " <> msg
      errorMessage (UnknownTool msg) = "Unknown tool: " <> msg
      errorMessage (InvalidRequest msg) = "Invalid request: " <> msg
      errorMessage (MethodNotFound msg) = "Method not found: " <> msg
      errorMessage (InvalidParams msg) = "Invalid parameters: " <> msg

-- | Prompt definition (2025-06-18 enhanced)
data PromptDefinition = PromptDefinition
  { promptDefinitionName        :: Text
  , promptDefinitionDescription :: Text
  , promptDefinitionArguments   :: [ArgumentDefinition]
  , promptDefinitionTitle       :: Maybe Text  -- New title field for human-friendly display
  } deriving (Show, Eq, Generic)

instance ToJSON PromptDefinition where
  toJSON def = object $
    [ "name" .= promptDefinitionName def
    , "description" .= promptDefinitionDescription def
    , "arguments" .= promptDefinitionArguments def
    ] ++ maybe [] (\t -> ["title" .= t]) (promptDefinitionTitle def)

-- | Resource definition (2025-06-18 enhanced)
data ResourceDefinition = ResourceDefinition
  { resourceDefinitionURI         :: Text
  , resourceDefinitionName        :: Text
  , resourceDefinitionDescription :: Maybe Text
  , resourceDefinitionMimeType    :: Maybe Text
  , resourceDefinitionTitle       :: Maybe Text  -- New title field for human-friendly display
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceDefinition where
  toJSON def = object $
    [ "uri" .= resourceDefinitionURI def
    , "name" .= resourceDefinitionName def
    ] ++
    maybe [] (\d -> ["description" .= d]) (resourceDefinitionDescription def) ++
    maybe [] (\m -> ["mimeType" .= m]) (resourceDefinitionMimeType def) ++
    maybe [] (\t -> ["title" .= t]) (resourceDefinitionTitle def)

-- | Tool definition (2025-06-18 enhanced)
data ToolDefinition = ToolDefinition
  { toolDefinitionName        :: Text
  , toolDefinitionDescription :: Text
  , toolDefinitionInputSchema :: InputSchemaDefinition
  , toolDefinitionTitle       :: Maybe Text  -- New title field for human-friendly display
  } deriving (Show, Eq, Generic)

instance ToJSON ToolDefinition where
  toJSON def = object $
    [ "name" .= toolDefinitionName def
    , "description" .= toolDefinitionDescription def
    , "inputSchema" .= toolDefinitionInputSchema def
    ] ++ maybe [] (\t -> ["title" .= t]) (toolDefinitionTitle def)

-- | Argument definition for prompts
data ArgumentDefinition = ArgumentDefinition
  { argumentDefinitionName        :: Text
  , argumentDefinitionDescription :: Text
  , argumentDefinitionRequired    :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ArgumentDefinition where
  toJSON def = object
    [ "name" .= argumentDefinitionName def
    , "description" .= argumentDefinitionDescription def
    , "required" .= argumentDefinitionRequired def
    ]

-- | Input schema definition for tools
data InputSchemaDefinition = InputSchemaDefinitionObject
  { properties :: [(Text, InputSchemaDefinitionProperty)]
  , required   :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON InputSchemaDefinition where
  toJSON (InputSchemaDefinitionObject props req) = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object (map (\(k, v) -> fromText k .= v) props)
    , "required" .= req
    ]

data InputSchemaDefinitionProperty = InputSchemaDefinitionProperty
  { propertyType        :: Text
  , propertyDescription :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON InputSchemaDefinitionProperty where
  toJSON prop = object
    [ "type" .= propertyType prop
    , "description" .= propertyDescription prop
    ]

-- | Server information
data McpServerInfo = McpServerInfo
  { serverName         :: Text
  , serverVersion      :: Text
  , serverInstructions :: Text
  } deriving (Show, Eq, Generic)

-- | Individual capability objects
data PromptCapabilities = PromptCapabilities
  { promptListChanged :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON PromptCapabilities where
  toJSON caps = object $ catMaybes
    [ fmap ("listChanged" .=) (promptListChanged caps)
    ]

data ResourceCapabilities = ResourceCapabilities
  { resourceSubscribe   :: Maybe Bool
  , resourceListChanged :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceCapabilities where
  toJSON caps = object $ catMaybes
    [ fmap ("subscribe" .=) (resourceSubscribe caps)
    , fmap ("listChanged" .=) (resourceListChanged caps)
    ]

data ToolCapabilities = ToolCapabilities
  { toolListChanged :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ToolCapabilities where
  toJSON caps = object $ catMaybes
    [ fmap ("listChanged" .=) (toolListChanged caps)
    ]

data LoggingCapabilities = LoggingCapabilities
  { -- No specific sub-capabilities for logging yet
  } deriving (Show, Eq, Generic)

instance ToJSON LoggingCapabilities where
  toJSON _ = object []

-- | Server capabilities
data ServerCapabilities = ServerCapabilities
  { capabilityPrompts   :: Maybe PromptCapabilities
  , capabilityResources :: Maybe ResourceCapabilities
  , capabilityTools     :: Maybe ToolCapabilities
  , capabilityLogging   :: Maybe LoggingCapabilities
  } deriving (Show, Eq, Generic)

instance ToJSON ServerCapabilities where
  toJSON caps = object $ catMaybes
    [ fmap ("prompts" .=) (capabilityPrompts caps)
    , fmap ("resources" .=) (capabilityResources caps)
    , fmap ("tools" .=) (capabilityTools caps)
    , fmap ("logging" .=) (capabilityLogging caps)
    ]


-- | Handler type definitions
type PromptListHandler m = m [PromptDefinition]
type PromptGetHandler m = PromptName -> [(ArgumentName, ArgumentValue)] -> m (Either Error Content)

type ResourceListHandler m = m [ResourceDefinition]
type ResourceReadHandler m = URI -> m (Either Error ResourceContent)

type ToolListHandler m = m [ToolDefinition]
type ToolCallHandler m = ToolName -> [(ArgumentName, ArgumentValue)] -> m (Either Error Content)

-- | Server handlers
data McpServerHandlers m = McpServerHandlers
  { prompts   :: Maybe (PromptListHandler m, PromptGetHandler m)
  , resources :: Maybe (ResourceListHandler m, ResourceReadHandler m)
  , tools     :: Maybe (ToolListHandler m, ToolCallHandler m)
  }
