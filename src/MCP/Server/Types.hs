{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.Types
  ( -- * Content Types
    Content(..)
  , ContentImageData(..)
  , ContentAudioData(..)
  , ResourceContent(..)

    -- * Metadata Types
  , Annotations(..)
  , defaultAnnotations
  , ToolAnnotations(..)
  , defaultToolAnnotations
  , Icon(..)
  , icon
  , LogLevel(..)

    -- * Handler Result Types
  , ToolResult(..)
  , toolResult
  , toolError
  , ToToolResult(..)
  , ToolOutput(..)
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
  , mkPromptDefinition
  , ResourceDefinition(..)
  , mkResourceDefinition
  , ResourceTemplateDefinition(..)
  , mkResourceTemplateDefinition
  , ToolDefinition(..)
  , mkToolDefinition
  , ArgumentDefinition(..)

    -- * Completion Types
  , CompletionRef(..)
  , CompletionResult(..)
  , completionResult

    -- * Server Types
  , McpServerInfo(..)
  , McpServerHandlers(..)
  , noHandlers
  , ClientContext(..)
  , anonymousContext
  , CacheHints(..)
  , defaultCacheHints
  , NotificationSupport(..)
  , noNotificationSupport
  , ServerCapabilities(..)
  , PromptCapabilities(..)
  , ResourceCapabilities(..)
  , ToolCapabilities(..)
  , CompletionCapabilities(..)
  , LoggingCapabilities(..)

    -- * Handler Types
  , PromptListHandler
  , PromptGetHandler
  , ResourceListHandler
  , ResourceReadHandler
  , ResourceTemplateListHandler
  , ToolListHandler
  , ToolCallHandler
  , CompletionHandler

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
import           Language.Haskell.TH.Syntax (Lift)
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
  | ContentAnnotated Annotations Content
    -- ^ A content block carrying 'Annotations'; the annotations are merged
    -- into the inner block's JSON object. Do not nest.
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
  toJSON (ContentAnnotated anns inner) =
    case toJSON inner of
      Object o -> Object (KM.insert "annotations" (toJSON anns) o)
      other    -> other

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    inner <- parseInner o
    case KM.lookup "annotations" o of
      Just anns -> ContentAnnotated <$> parseJSON anns <*> pure inner
      Nothing   -> pure inner
    where
      parseInner o = do
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

-- | Optional hints on content blocks: who the content is for, how important
-- it is, and when it last changed (2025-03-26+).
data Annotations = Annotations
  { annotationsAudience     :: [MessageRole]  -- ^ Intended audience(s); omitted when empty
  , annotationsPriority     :: Maybe Double   -- ^ 0.0 (optional) … 1.0 (most important)
  , annotationsLastModified :: Maybe Text     -- ^ ISO 8601 timestamp
  } deriving (Show, Eq, Generic)

-- | No annotations set; record-update the ones you need.
defaultAnnotations :: Annotations
defaultAnnotations = Annotations
  { annotationsAudience = []
  , annotationsPriority = Nothing
  , annotationsLastModified = Nothing
  }

instance ToJSON Annotations where
  toJSON anns = object $
    (if null (annotationsAudience anns) then [] else ["audience" .= annotationsAudience anns])
    ++ maybe [] (\p -> ["priority" .= p]) (annotationsPriority anns)
    ++ maybe [] (\lm -> ["lastModified" .= lm]) (annotationsLastModified anns)

instance FromJSON Annotations where
  parseJSON = withObject "Annotations" $ \o -> Annotations
    <$> o .:? "audience" .!= []
    <*> o .:? "priority"
    <*> o .:? "lastModified"

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

instance FromJSON MessageRole where
  parseJSON = withText "MessageRole" $ \t -> case t of
    "user"      -> pure RoleUser
    "assistant" -> pure RoleAssistant
    _           -> fail $ "Unknown role: " ++ T.unpack t

-- | Behavioral hints on a tool (2025-03-26+): clients use these for
-- permission UX (e.g. auto-approving read-only tools). All hints are
-- advisory and default to unset.
data ToolAnnotations = ToolAnnotations
  { toolAnnotationsTitle      :: Maybe Text
  , toolReadOnlyHint          :: Maybe Bool  -- ^ The tool does not modify its environment
  , toolDestructiveHint       :: Maybe Bool  -- ^ The tool may perform destructive updates
  , toolIdempotentHint        :: Maybe Bool  -- ^ Repeated calls with the same arguments have no additional effect
  , toolOpenWorldHint         :: Maybe Bool  -- ^ The tool interacts with an open world of external entities
  } deriving (Show, Eq, Generic, Lift)

-- | No hints set; record-update the ones you need.
defaultToolAnnotations :: ToolAnnotations
defaultToolAnnotations = ToolAnnotations
  { toolAnnotationsTitle = Nothing
  , toolReadOnlyHint = Nothing
  , toolDestructiveHint = Nothing
  , toolIdempotentHint = Nothing
  , toolOpenWorldHint = Nothing
  }

instance ToJSON ToolAnnotations where
  toJSON anns = object $ concat
    [ maybe [] (\t -> ["title" .= t]) (toolAnnotationsTitle anns)
    , maybe [] (\b -> ["readOnlyHint" .= b]) (toolReadOnlyHint anns)
    , maybe [] (\b -> ["destructiveHint" .= b]) (toolDestructiveHint anns)
    , maybe [] (\b -> ["idempotentHint" .= b]) (toolIdempotentHint anns)
    , maybe [] (\b -> ["openWorldHint" .= b]) (toolOpenWorldHint anns)
    ]

-- | An icon a client may display for a tool, prompt or resource
-- (2025-11-25+).
data Icon = Icon
  { iconSrc      :: Text        -- ^ URI of the icon
  , iconMimeType :: Maybe Text
  , iconSizes    :: [Text]      -- ^ e.g. @[\"48x48\"]@; omitted when empty
  } deriving (Show, Eq, Generic, Lift)

-- | An icon with just a source URI.
icon :: Text -> Icon
icon src = Icon { iconSrc = src, iconMimeType = Nothing, iconSizes = [] }

-- | RFC 5424 log severities, least to most severe; the 'Ord' instance
-- follows severity, so @level >= threshold@ is the filtering test.
data LogLevel
  = LogDebug
  | LogInfo
  | LogNotice
  | LogWarning
  | LogError
  | LogCritical
  | LogAlert
  | LogEmergency
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

logLevelText :: LogLevel -> Text
logLevelText l = case l of
  LogDebug     -> "debug"
  LogInfo      -> "info"
  LogNotice    -> "notice"
  LogWarning   -> "warning"
  LogError     -> "error"
  LogCritical  -> "critical"
  LogAlert     -> "alert"
  LogEmergency -> "emergency"

instance ToJSON LogLevel where
  toJSON = String . logLevelText

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ \t ->
    case lookup t [(logLevelText l, l) | l <- [minBound .. maxBound]] of
      Just l  -> pure l
      Nothing -> fail $ "Unknown log level: " ++ T.unpack t

instance ToJSON Icon where
  toJSON i = object $
    [ "src" .= iconSrc i ]
    ++ maybe [] (\m -> ["mimeType" .= m]) (iconMimeType i)
    ++ (if null (iconSizes i) then [] else ["sizes" .= iconSizes i])

instance FromJSON Icon where
  parseJSON = withObject "Icon" $ \o -> Icon
    <$> o .: "src"
    <*> o .:? "mimeType"
    <*> o .:? "sizes" .!= []

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

-- | What a tool handler derived with
-- 'MCP.Server.Derive.deriveToolHandlerWithOutput' returns: a typed value
-- that the derivation serializes into @structuredContent@ (matching the
-- derived @outputSchema@), or one of the escape hatches.
data ToolOutput o
  = ToolOutput o
    -- ^ A structured value. Per the spec's recommendation, the serialized
    -- JSON is also returned as a text content block for clients that
    -- predate structured output.
  | ToolOutputWith [Content] o
    -- ^ A structured value with caller-supplied content blocks (no
    -- automatic text block).
  | ToolOutputError Text
    -- ^ An execution failure, reported with @isError@ (see 'toolError').
  | ToolOutputRaw ToolResult
    -- ^ Full control: return this 'ToolResult' as-is, with no structured
    -- content implied.
  deriving (Show, Eq)

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
  , promptDefinitionIcons       :: [Icon]      -- ^ omitted when empty (2025-11-25+)
  } deriving (Show, Eq, Generic)

-- | A prompt definition with only the required fields set; record-update
-- the optional ones (constructing 'PromptDefinition' directly breaks when
-- fields are added).
mkPromptDefinition :: Text -> Text -> [ArgumentDefinition] -> PromptDefinition
mkPromptDefinition name description args = PromptDefinition
  { promptDefinitionName = name
  , promptDefinitionDescription = description
  , promptDefinitionArguments = args
  , promptDefinitionTitle = Nothing
  , promptDefinitionIcons = []
  }

instance ToJSON PromptDefinition where
  toJSON def = object $
    [ "name" .= promptDefinitionName def
    , "description" .= promptDefinitionDescription def
    , "arguments" .= promptDefinitionArguments def
    ] ++ maybe [] (\t -> ["title" .= t]) (promptDefinitionTitle def)
      ++ (if null (promptDefinitionIcons def) then [] else ["icons" .= promptDefinitionIcons def])

-- | Resource definition (2025-06-18 enhanced)
data ResourceDefinition = ResourceDefinition
  { resourceDefinitionURI         :: Text
  , resourceDefinitionName        :: Text
  , resourceDefinitionDescription :: Maybe Text
  , resourceDefinitionMimeType    :: Maybe Text
  , resourceDefinitionTitle       :: Maybe Text  -- New title field for human-friendly display
  , resourceDefinitionIcons       :: [Icon]      -- ^ omitted when empty (2025-11-25+)
  } deriving (Show, Eq, Generic)

-- | A resource definition with only the required fields set; record-update
-- the optional ones.
mkResourceDefinition :: Text -> Text -> ResourceDefinition
mkResourceDefinition uri name = ResourceDefinition
  { resourceDefinitionURI = uri
  , resourceDefinitionName = name
  , resourceDefinitionDescription = Nothing
  , resourceDefinitionMimeType = Nothing
  , resourceDefinitionTitle = Nothing
  , resourceDefinitionIcons = []
  }

instance ToJSON ResourceDefinition where
  toJSON def = object $
    [ "uri" .= resourceDefinitionURI def
    , "name" .= resourceDefinitionName def
    ] ++
    maybe [] (\d -> ["description" .= d]) (resourceDefinitionDescription def) ++
    maybe [] (\m -> ["mimeType" .= m]) (resourceDefinitionMimeType def) ++
    maybe [] (\t -> ["title" .= t]) (resourceDefinitionTitle def) ++
    (if null (resourceDefinitionIcons def) then [] else ["icons" .= resourceDefinitionIcons def])

instance FromJSON ResourceDefinition where
  parseJSON = withObject "ResourceDefinition" $ \o -> ResourceDefinition
    <$> o .: "uri"
    <*> o .: "name"
    <*> o .:? "description"
    <*> o .:? "mimeType"
    <*> o .:? "title"
    <*> o .:? "icons" .!= []

-- | Resource template definition: a parameterized resource identified by an
-- RFC 6570 URI template.
data ResourceTemplateDefinition = ResourceTemplateDefinition
  { resourceTemplateURITemplate :: Text
  , resourceTemplateName        :: Text
  , resourceTemplateDescription :: Maybe Text
  , resourceTemplateMimeType    :: Maybe Text
  , resourceTemplateTitle       :: Maybe Text
  , resourceTemplateIcons       :: [Icon]  -- ^ omitted when empty (2025-11-25+)
  } deriving (Show, Eq, Generic)

-- | A template definition with only the required fields set; record-update
-- the optional ones.
mkResourceTemplateDefinition :: Text -> Text -> ResourceTemplateDefinition
mkResourceTemplateDefinition uriTemplate name = ResourceTemplateDefinition
  { resourceTemplateURITemplate = uriTemplate
  , resourceTemplateName = name
  , resourceTemplateDescription = Nothing
  , resourceTemplateMimeType = Nothing
  , resourceTemplateTitle = Nothing
  , resourceTemplateIcons = []
  }

instance ToJSON ResourceTemplateDefinition where
  toJSON def = object $
    [ "uriTemplate" .= resourceTemplateURITemplate def
    , "name" .= resourceTemplateName def
    ] ++
    maybe [] (\d -> ["description" .= d]) (resourceTemplateDescription def) ++
    maybe [] (\m -> ["mimeType" .= m]) (resourceTemplateMimeType def) ++
    maybe [] (\t -> ["title" .= t]) (resourceTemplateTitle def) ++
    (if null (resourceTemplateIcons def) then [] else ["icons" .= resourceTemplateIcons def])

-- | What a completion request is completing an argument for.
data CompletionRef
  = CompletionRefPrompt Text    -- ^ @ref/prompt@: a prompt, by name
  | CompletionRefResource Text  -- ^ @ref/resource@: a resource URI or URI template
  deriving (Show, Eq, Generic)

-- | Completion suggestions for an argument value.
data CompletionResult = CompletionResult
  { completionValues  :: [Text]      -- ^ Suggestions ranked by relevance (max 100 are sent)
  , completionTotal   :: Maybe Int   -- ^ Optional total number of matches
  , completionHasMore :: Maybe Bool  -- ^ Whether more results exist beyond 'completionValues'
  } deriving (Show, Eq, Generic)

-- | A completion result with just the given suggestions.
completionResult :: [Text] -> CompletionResult
completionResult vals = CompletionResult
  { completionValues = vals
  , completionTotal = Nothing
  , completionHasMore = Nothing
  }

-- | Tool definition (2025-06-18 enhanced)
data ToolDefinition = ToolDefinition
  { toolDefinitionName         :: Text
  , toolDefinitionDescription  :: Text
  , toolDefinitionInputSchema  :: Schema
  , toolDefinitionOutputSchema :: Maybe Schema
  , toolDefinitionTitle        :: Maybe Text  -- New title field for human-friendly display
  , toolDefinitionAnnotations  :: Maybe ToolAnnotations  -- ^ behavioral hints (2025-03-26+)
  , toolDefinitionIcons        :: [Icon]                 -- ^ omitted when empty (2025-11-25+)
  } deriving (Show, Eq, Generic)

-- | A tool definition with only the required fields set; record-update the
-- optional ones.
mkToolDefinition :: Text -> Text -> Schema -> ToolDefinition
mkToolDefinition name description inputSchema = ToolDefinition
  { toolDefinitionName = name
  , toolDefinitionDescription = description
  , toolDefinitionInputSchema = inputSchema
  , toolDefinitionOutputSchema = Nothing
  , toolDefinitionTitle = Nothing
  , toolDefinitionAnnotations = Nothing
  , toolDefinitionIcons = []
  }

instance ToJSON ToolDefinition where
  toJSON def = object $
    [ "name" .= toolDefinitionName def
    , "description" .= toolDefinitionDescription def
    , "inputSchema" .= toolDefinitionInputSchema def
    ] ++ maybe [] (\s -> ["outputSchema" .= s]) (toolDefinitionOutputSchema def)
      ++ maybe [] (\t -> ["title" .= t]) (toolDefinitionTitle def)
      ++ maybe [] (\a -> ["annotations" .= a]) (toolDefinitionAnnotations def)
      ++ (if null (toolDefinitionIcons def) then [] else ["icons" .= toolDefinitionIcons def])

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

data CompletionCapabilities = CompletionCapabilities
  { -- No sub-capabilities defined
  } deriving (Show, Eq, Generic)

instance ToJSON CompletionCapabilities where
  toJSON _ = object []

data LoggingCapabilities = LoggingCapabilities
  { -- No specific sub-capabilities for logging yet
  } deriving (Show, Eq, Generic)

instance ToJSON LoggingCapabilities where
  toJSON _ = object []

-- | Server capabilities
data ServerCapabilities = ServerCapabilities
  { capabilityPrompts     :: Maybe PromptCapabilities
  , capabilityResources   :: Maybe ResourceCapabilities
  , capabilityTools       :: Maybe ToolCapabilities
  , capabilityCompletions :: Maybe CompletionCapabilities
  , capabilityLogging     :: Maybe LoggingCapabilities
  } deriving (Show, Eq, Generic)

instance ToJSON ServerCapabilities where
  toJSON caps = object $ catMaybes
    [ fmap ("prompts" .=) (capabilityPrompts caps)
    , fmap ("resources" .=) (capabilityResources caps)
    , fmap ("tools" .=) (capabilityTools caps)
    , fmap ("completions" .=) (capabilityCompletions caps)
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
  , clientProtocolVersion :: Maybe Text
      -- ^ The protocol revision the request declared in its @_meta@
      --   (modern, 2026-07-28+ clients). 'Nothing' for legacy clients that
      --   negotiated via @initialize@.
  , clientInfo :: Maybe Value
      -- ^ The client's self-reported identity from request @_meta@
      --   (modern clients), for display/logging only.
  , clientCapabilities :: Maybe Value
      -- ^ The client's declared capabilities from request @_meta@
      --   (modern clients).
  , reportProgress :: Double -> Maybe Double -> Maybe Text -> IO ()
      -- ^ Report progress for this request: current progress (which must
      --   increase with each call), an optional total, and an optional
      --   human-readable message. A no-op when the request carried no
      --   @progressToken@, so handlers can call it unconditionally. Stop
      --   reporting once the handler returns; avoid flooding.
  , logToClient :: LogLevel -> Value -> IO ()
      -- ^ Send a log message to this request's client
      --   (@notifications\/message@). A no-op unless the request declared
      --   @io.modelcontextprotocol\/logLevel@ (the spec forbids emitting
      --   otherwise); messages below the declared level are dropped.
  }

-- | A context carrying no transport- or request-level information: what
-- handlers see for legacy stdio requests.
anonymousContext :: ClientContext
anonymousContext = ClientContext
  { clientToken = Nothing
  , clientPrincipal = Nothing
  , clientProtocolVersion = Nothing
  , clientInfo = Nothing
  , clientCapabilities = Nothing
  , reportProgress = \_ _ _ -> pure ()
  , logToClient = \_ _ -> pure ()
  }

-- | Cacheability hints stamped onto modern (2026-07-28+) list/read results,
-- which require @ttlMs@ and @cacheScope@ fields.
data CacheHints = CacheHints
  { cacheTtlMs       :: Int   -- ^ Freshness hint in milliseconds.
  , cacheScopePublic :: Bool  -- ^ 'True' allows shared intermediaries to
                              --   cache the response (@\"public\"@);
                              --   'False' is @\"private\"@.
  } deriving (Show, Eq)

-- | Conservative defaults: no caching (@ttlMs = 0@), private scope.
defaultCacheHints :: CacheHints
defaultCacheHints = CacheHints
  { cacheTtlMs = 0
  , cacheScopePublic = False
  }

-- | What change-notification delivery the serving transport offers, which
-- decides the @listChanged@\/@subscribe@ capabilities each era advertises.
data NotificationSupport = NotificationSupport
  { supportsLegacyPush :: Bool
      -- ^ The transport can push unsolicited notifications to legacy
      --   (initialize-handshake) clients — true for stdio with a configured
      --   notification source; never for HTTP (this library dropped the
      --   deprecated GET SSE stream legacy delivery relied on).
  , supportsListen :: Bool
      -- ^ @subscriptions\/listen@ (2026-07-28) is served.
  } deriving (Show, Eq)

-- | No notification delivery at all: nothing extra is advertised.
noNotificationSupport :: NotificationSupport
noNotificationSupport = NotificationSupport
  { supportsLegacyPush = False
  , supportsListen = False
  }

-- | Handler type definitions. Every handler receives the request's
-- 'ClientContext' as its first argument.
--
-- Prompt arguments are string-valued per the MCP specification; tool
-- arguments are full JSON values.
type PromptListHandler = ClientContext -> IO [PromptDefinition]
type PromptGetHandler = ClientContext -> PromptName -> Map Text Text -> IO (Either Error PromptResult)

type ResourceListHandler = ClientContext -> IO [ResourceDefinition]
type ResourceReadHandler = ClientContext -> URI -> IO (Either Error ResourceContent)
type ResourceTemplateListHandler = ClientContext -> IO [ResourceTemplateDefinition]

type ToolListHandler = ClientContext -> IO [ToolDefinition]
type ToolCallHandler = ClientContext -> ToolName -> Map Text Value -> IO (Either Error ToolResult)

-- | Completion handler: given what is being completed ('CompletionRef'), the
-- argument name, the partial value typed so far, and any already-resolved
-- sibling arguments, produce ranked suggestions.
type CompletionHandler = ClientContext -> CompletionRef -> ArgumentName -> Text -> Map Text Text -> IO (Either Error CompletionResult)

-- | Server handlers
data McpServerHandlers = McpServerHandlers
  { prompts           :: Maybe (PromptListHandler, PromptGetHandler)
  , resources         :: Maybe (ResourceListHandler, ResourceReadHandler)
  , resourceTemplates :: Maybe ResourceTemplateListHandler
      -- ^ Parameterized resources (@resources\/templates\/list@). Template
      --   URIs are read through the 'ResourceReadHandler' in 'resources' —
      --   configure both, or template reads have nothing to serve them
      --   (a templates-only configuration still answers @resources\/list@
      --   with an empty list so the advertised capability stays honest).
  , tools             :: Maybe (ToolListHandler, ToolCallHandler)
  , completions       :: Maybe CompletionHandler
      -- ^ Argument autocompletion (@completion\/complete@) for prompts and
      --   resource templates.
  }

-- | Handlers for a server that supports nothing — record-update the features
-- you provide, so adding new handler slots to the library does not break
-- your construction.
noHandlers :: McpServerHandlers
noHandlers = McpServerHandlers
  { prompts = Nothing
  , resources = Nothing
  , resourceTemplates = Nothing
  , tools = Nothing
  , completions = Nothing
  }
