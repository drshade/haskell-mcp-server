{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.Types
  ( -- * Content Types
    Content(..)
  , ContentImageData(..)
  , ContentAudioData(..)
  , ResourceContent(..)

    -- * Handler Result Types
  , ToolResult(..)
  , toolResult
  , toolError
  , ToToolResult(..)
  , PromptResult(..)
  , ToPromptResult(..)
  , PromptMessage(..)
  , MessageRole(..)

    -- * URI Utilities
  , parseURI
  , URI

    -- * Error Types
  , Error(..)

    -- * Schema Types
  , Schema(..)
  , SchemaType(..)
  , schema
  , describedSchema

    -- * Definition Types
  , PromptDefinition(..)
  , ResourceDefinition(..)
  , ToolDefinition(..)
  , ArgumentDefinition(..)

    -- * Server Types
  , McpServerInfo(..)
  , McpServerHandlers(..)
  , ClientContext(..)
  , ServerCapabilities(..)
  , PromptCapabilities(..)
  , ResourceCapabilities(..)
  , ToolCapabilities(..)
  , LoggingCapabilities(..)

    -- * Handler Types
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
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types (Pair, Parser)
import           Data.Map         (Map)
import           Data.Maybe       (catMaybes, listToMaybe)
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
  | ContentAudio ContentAudioData
  | ContentEmbeddedResource ResourceContent
    -- ^ A resource embedded into the result, carrying its full contents
  | ContentResourceLink ResourceDefinition
    -- ^ A reference to a resource the client can read separately
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
  toJSON (ContentAudio audio) = object
    [ "type" .= ("audio" :: Text)
    , "data" .= contentAudioData audio
    , "mimeType" .= contentAudioMimeType audio
    ]
  toJSON (ContentEmbeddedResource res) = object
    [ "type" .= ("resource" :: Text)
    , "resource" .= res
    ]
  toJSON (ContentResourceLink def) =
    case toJSON def of
      Object o -> Object (KM.insert "type" (String "resource_link") o)
      other    -> other

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    contentType <- o .: "type" :: Parser Text
    case contentType of
      "text" -> ContentText <$> o .: "text"
      "image" -> do
        imgData <- o .: "data"
        mimeType <- o .: "mimeType"
        return $ ContentImage $ ContentImageData imgData mimeType
      "audio" -> do
        audioData <- o .: "data"
        mimeType <- o .: "mimeType"
        return $ ContentAudio $ ContentAudioData audioData mimeType
      "resource" -> ContentEmbeddedResource <$> o .: "resource"
      "resource_link" -> ContentResourceLink <$> parseJSON (Object o)
      _ -> fail $ "Unknown content type: " ++ T.unpack contentType

data ContentImageData = ContentImageData
  { contentImageData     :: Text  -- ^ base64-encoded image data
  , contentImageMimeType :: Text
  } deriving (Show, Eq, Generic)

data ContentAudioData = ContentAudioData
  { contentAudioData     :: Text  -- ^ base64-encoded audio data
  , contentAudioMimeType :: Text
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

-- | Message role for prompts
data MessageRole = RoleUser | RoleAssistant
  deriving (Show, Eq, Generic)

instance ToJSON MessageRole where
  toJSON RoleUser      = "user"
  toJSON RoleAssistant = "assistant"

-- | Prompt message
data PromptMessage = PromptMessage
  { promptMessageRole    :: MessageRole
  , promptMessageContent :: Content
  } deriving (Show, Eq, Generic)

instance ToJSON PromptMessage where
  toJSON msg = object
    [ "role" .= promptMessageRole msg
    , "content" .= promptMessageContent msg
    ]

-- | The full result of a tool call.
--
-- Tool /execution/ failures belong in the result with 'toolResultIsError'
-- set (so the model can see what went wrong and react); JSON-RPC errors are
-- reserved for protocol-level failures such as unknown tools or malformed
-- arguments.
data ToolResult = ToolResult
  { toolResultContent    :: [Content]
  , toolResultStructured :: Maybe Value  -- ^ structuredContent (2025-06-18+)
  , toolResultIsError    :: Bool
  , toolResultMeta       :: Maybe Value
  } deriving (Show, Eq, Generic)

-- | A successful tool result with the given content blocks.
toolResult :: [Content] -> ToolResult
toolResult content = ToolResult
  { toolResultContent = content
  , toolResultStructured = Nothing
  , toolResultIsError = False
  , toolResultMeta = Nothing
  }

-- | A failed tool execution: the message is reported to the model with
-- @isError: true@ rather than as a JSON-RPC error.
toolError :: Text -> ToolResult
toolError msg = (toolResult [ContentText msg]) { toolResultIsError = True }

-- | Types a tool handler may return. Returning plain 'Content' (or 'Text')
-- keeps simple handlers simple; return a full 'ToolResult' for multiple
-- content blocks, structured content, or execution errors.
class ToToolResult a where
  toToolResult :: a -> ToolResult

instance ToToolResult ToolResult where
  toToolResult = id

instance ToToolResult Content where
  toToolResult c = toolResult [c]

-- | Concatenates content; a merged result is an error if any element is
-- ('toolResultIsError' is OR-ed), and the first present 'structuredContent'
-- and @_meta@ are kept.
instance ToToolResult a => ToToolResult [a] where
  toToolResult xs = ToolResult
    { toolResultContent = concatMap toolResultContent rs
    , toolResultStructured = firstJust (map toolResultStructured rs)
    , toolResultIsError = any toolResultIsError rs
    , toolResultMeta = firstJust (map toolResultMeta rs)
    }
    where rs = map toToolResult xs

instance ToToolResult Text where
  toToolResult = toToolResult . ContentText

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

-- | The full result of a prompts/get request: an optional description and
-- a conversation of one or more messages.
data PromptResult = PromptResult
  { promptResultDescription :: Maybe Text
  , promptResultMessages    :: [PromptMessage]
  } deriving (Show, Eq, Generic)

-- | Types a prompt handler may return. Plain 'Content' (or 'Text') becomes a
-- single user message; return 'PromptResult' or @['PromptMessage']@ for
-- multi-message conversations or assistant roles.
class ToPromptResult a where
  toPromptResult :: a -> PromptResult

instance ToPromptResult PromptResult where
  toPromptResult = id

instance ToPromptResult PromptMessage where
  toPromptResult m = PromptResult Nothing [m]

-- | Concatenates messages and keeps the first present description.
instance ToPromptResult a => ToPromptResult [a] where
  toPromptResult xs = PromptResult
    (firstJust (map promptResultDescription rs))
    (concatMap promptResultMessages rs)
    where rs = map toPromptResult xs

instance ToPromptResult Content where
  toPromptResult c = toPromptResult (PromptMessage RoleUser c)

instance ToPromptResult Text where
  toPromptResult = toPromptResult . ContentText

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

-- | A JSON Schema fragment: a shape plus an optional description.
data Schema = Schema
  { schemaDescription :: Maybe Text
  , schemaShape       :: SchemaType
  } deriving (Show, Eq, Generic)

-- | The shape of a JSON Schema fragment.
data SchemaType
  = SchemaString (Maybe [Text])
    -- ^ A string, optionally restricted to an enum of allowed values
  | SchemaInteger
  | SchemaNumber
  | SchemaBoolean
  | SchemaArray Schema
  | SchemaObject [(Text, Schema)] [Text]
    -- ^ Properties and the names of the required ones
  deriving (Show, Eq, Generic)

-- | A schema with no description.
schema :: SchemaType -> Schema
schema = Schema Nothing

-- | A schema with a description.
describedSchema :: Text -> SchemaType -> Schema
describedSchema desc = Schema (Just desc)

instance ToJSON Schema where
  toJSON (Schema desc shape) = object $
    typeFields shape ++ maybe [] (\d -> ["description" .= d]) desc
    where
      typeFields :: SchemaType -> [Pair]
      typeFields (SchemaString enumVals) =
        [ "type" .= ("string" :: Text) ]
        ++ maybe [] (\vs -> ["enum" .= vs]) enumVals
      typeFields SchemaInteger = [ "type" .= ("integer" :: Text) ]
      typeFields SchemaNumber  = [ "type" .= ("number" :: Text) ]
      typeFields SchemaBoolean = [ "type" .= ("boolean" :: Text) ]
      typeFields (SchemaArray items) =
        [ "type" .= ("array" :: Text)
        , "items" .= items
        ]
      typeFields (SchemaObject props req) =
        [ "type" .= ("object" :: Text)
        , "properties" .= object (map (\(k, v) -> fromText k .= v) props)
        , "required" .= req
        ]

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

instance FromJSON ResourceDefinition where
  parseJSON = withObject "ResourceDefinition" $ \o -> ResourceDefinition
    <$> o .: "uri"
    <*> o .: "name"
    <*> o .:? "description"
    <*> o .:? "mimeType"
    <*> o .:? "title"

-- | Tool definition (2025-06-18 enhanced)
data ToolDefinition = ToolDefinition
  { toolDefinitionName         :: Text
  , toolDefinitionDescription  :: Text
  , toolDefinitionInputSchema  :: Schema
  , toolDefinitionOutputSchema :: Maybe Schema
  , toolDefinitionTitle        :: Maybe Text  -- New title field for human-friendly display
  } deriving (Show, Eq, Generic)

instance ToJSON ToolDefinition where
  toJSON def = object $
    [ "name" .= toolDefinitionName def
    , "description" .= toolDefinitionDescription def
    , "inputSchema" .= toolDefinitionInputSchema def
    ] ++ maybe [] (\s -> ["outputSchema" .= s]) (toolDefinitionOutputSchema def)
      ++ maybe [] (\t -> ["title" .= t]) (toolDefinitionTitle def)

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


-- | Per-request context passed to every handler, so handlers can behave
-- differently depending on who is calling.
data ClientContext = ClientContext
  { clientToken     :: Maybe Text   -- ^ Authenticated bearer token, if any.
  , clientPrincipal :: Maybe Value  -- ^ Application-defined principal returned by
                                    --   the transport's authorization callback
                                    --   (e.g. a role). 'Nothing' when
                                    --   authentication is disabled.
  } deriving (Show, Eq)

-- | Handler type definitions. Every handler receives the request's
-- 'ClientContext' as its first argument.
--
-- Prompt arguments are string-valued per the MCP specification; tool
-- arguments are full JSON values.
type PromptListHandler = ClientContext -> IO [PromptDefinition]
type PromptGetHandler = ClientContext -> PromptName -> Map Text Text -> IO (Either Error PromptResult)

type ResourceListHandler = ClientContext -> IO [ResourceDefinition]
type ResourceReadHandler = ClientContext -> URI -> IO (Either Error ResourceContent)

type ToolListHandler = ClientContext -> IO [ToolDefinition]
type ToolCallHandler = ClientContext -> ToolName -> Map Text Value -> IO (Either Error ToolResult)

-- | Server handlers
data McpServerHandlers = McpServerHandlers
  { prompts   :: Maybe (PromptListHandler, PromptGetHandler)
  , resources :: Maybe (ResourceListHandler, ResourceReadHandler)
  , tools     :: Maybe (ToolListHandler, ToolCallHandler)
  }
