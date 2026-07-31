{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.Protocol
  ( -- * MCP Protocol Messages
    InitializeRequest(..)
  , InitializeResponse(..)
  , InitializedNotification(..)
  , PingRequest(..)
  , PongResponse(..)

    -- * Prompts Protocol
  , PromptsListRequest(..)
  , PromptsListResponse(..)
  , PromptsGetRequest(..)
  , PromptsGetResponse(..)
  , PromptMessage(..)
  , MessageRole(..)

    -- * Resources Protocol
  , ResourcesListRequest(..)
  , ResourcesListResponse(..)
  , ResourcesReadRequest(..)
  , ResourcesReadResponse(..)

    -- * Tools Protocol
  , ToolsListRequest(..)
  , ToolsListResponse(..)
  , ToolsCallRequest(..)
  , ToolsCallResponse(..)

    -- * Common Types
  , ListChangedNotification(..)

    -- * Protocol Functions
  , protocolVersion
  , supportedVersions
  , modernVersions
  , allVersions
  ) where

import           Data.Aeson
import           Data.Map         (Map)
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           MCP.Server.Types

-- | The newest /legacy/ (initialize-handshake) revision the server
-- advertises by default. Used as the fallback when an initializing client
-- proposes a version this library does not recognise — deliberately capped
-- at the newest legacy revision: a client that sends @initialize@ is legacy
-- by definition and must not be promised the stateless 2026-07-28 semantics.
protocolVersion :: Text
protocolVersion = "2025-11-25"

-- | Legacy (initialize-handshake) revisions whose wire format for the basic
-- tool/resource/prompt operations this library implements is identical.
-- The server echoes back any of these a client proposes (see
-- 'MCP.Server.Handlers.validateProtocolVersion'), satisfying the spec
-- requirement that a supported version be answered with the same version.
--
-- Ordered newest-first.
supportedVersions :: [Text]
supportedVersions =
  [ "2025-11-25"
  , "2025-06-18"
  , "2025-03-26"
  , "2024-11-05"
  ]

-- | Modern (stateless, per-request @_meta@) revisions this library
-- implements. Requests declaring one of these are served with modern
-- semantics: @resultType@, cacheability fields, and @serverInfo@ in result
-- @_meta@.
modernVersions :: [Text]
modernVersions =
  [ "2026-07-28"
  ]

-- | Every revision this library implements, newest first — what
-- @server\/discover@ and 'UnsupportedProtocolVersionError' report.
allVersions :: [Text]
allVersions = modernVersions ++ supportedVersions


-- | Initialize request
data InitializeRequest = InitializeRequest
  { initProtocolVersion :: Text
  , initCapabilities    :: Value
  , initClientInfo      :: Value
  } deriving (Show, Eq, Generic)

instance FromJSON InitializeRequest where
  parseJSON = withObject "InitializeRequest" $ \o -> InitializeRequest
    <$> o .: "protocolVersion"
    <*> o .: "capabilities"
    <*> o .: "clientInfo"

-- | Initialize response
data InitializeResponse = InitializeResponse
  { initRespProtocolVersion :: Text
  , initRespCapabilities    :: ServerCapabilities
  , initRespServerInfo      :: McpServerInfo
  } deriving (Show, Eq, Generic)

instance ToJSON InitializeResponse where
  toJSON resp = object
    [ "protocolVersion" .= initRespProtocolVersion resp
    , "capabilities" .= initRespCapabilities resp
    , "serverInfo" .= object
        [ "name" .= serverName (initRespServerInfo resp)
        , "version" .= serverVersion (initRespServerInfo resp)
        , "instructions" .= serverInstructions (initRespServerInfo resp)
        ]
    ]

-- | Initialized notification (no parameters)
data InitializedNotification = InitializedNotification
  deriving (Show, Eq, Generic)

instance FromJSON InitializedNotification where
  parseJSON _ = return InitializedNotification

-- | Ping request (no parameters)
data PingRequest = PingRequest
  deriving (Show, Eq, Generic)

instance FromJSON PingRequest where
  parseJSON _ = return PingRequest

-- | Pong response (empty object)
data PongResponse = PongResponse
  deriving (Show, Eq, Generic)

instance ToJSON PongResponse where
  toJSON PongResponse = object []

-- | Prompts list request
data PromptsListRequest = PromptsListRequest
  deriving (Show, Eq, Generic)

instance FromJSON PromptsListRequest where
  parseJSON _ = return PromptsListRequest

-- | Prompts list response
data PromptsListResponse = PromptsListResponse
  { promptsListPrompts :: [PromptDefinition]
  } deriving (Show, Eq, Generic)

instance ToJSON PromptsListResponse where
  toJSON resp = object
    [ "prompts" .= promptsListPrompts resp
    ]

-- | Prompts get request
data PromptsGetRequest = PromptsGetRequest
  { promptsGetName      :: Text
  , promptsGetArguments :: Maybe (Map Text Value)
  } deriving (Show, Eq, Generic)

instance FromJSON PromptsGetRequest where
  parseJSON = withObject "PromptsGetRequest" $ \o -> PromptsGetRequest
    <$> o .: "name"
    <*> o .:? "arguments"

-- | Prompts get response (2025-06-18 enhanced)
data PromptsGetResponse = PromptsGetResponse
  { promptsGetDescription :: Maybe Text
  , promptsGetMessages    :: [PromptMessage]
  , promptsGetMeta        :: Maybe Value  -- New _meta field for additional metadata
  } deriving (Show, Eq, Generic)

instance ToJSON PromptsGetResponse where
  toJSON resp = object $
    [ "messages" .= promptsGetMessages resp
    ] ++ maybe [] (\d -> ["description" .= d]) (promptsGetDescription resp)
      ++ maybe [] (\m -> ["_meta" .= m]) (promptsGetMeta resp)

-- | Resources list request
data ResourcesListRequest = ResourcesListRequest
  deriving (Show, Eq, Generic)

instance FromJSON ResourcesListRequest where
  parseJSON _ = return ResourcesListRequest

-- | Resources list response
data ResourcesListResponse = ResourcesListResponse
  { resourcesListResources :: [ResourceDefinition]
  } deriving (Show, Eq, Generic)

instance ToJSON ResourcesListResponse where
  toJSON resp = object
    [ "resources" .= resourcesListResources resp
    ]

-- | Resources read request
data ResourcesReadRequest = ResourcesReadRequest
  { resourcesReadUri :: URI
  } deriving (Show, Eq, Generic)

instance FromJSON ResourcesReadRequest where
  parseJSON = withObject "ResourcesReadRequest" $ \o -> do
    uriText <- o .: "uri"
    case parseURI uriText of
      Just uri -> return $ ResourcesReadRequest uri
      Nothing  -> fail "Invalid URI"

-- | Resources read response
data ResourcesReadResponse = ResourcesReadResponse
  { resourcesReadContents :: [ResourceContent]
  } deriving (Show, Eq, Generic)

instance ToJSON ResourcesReadResponse where
  toJSON resp = object
    [ "contents" .= resourcesReadContents resp
    ]

-- | Tools list request
data ToolsListRequest = ToolsListRequest
  deriving (Show, Eq, Generic)

instance FromJSON ToolsListRequest where
  parseJSON _ = return ToolsListRequest

-- | Tools list response
data ToolsListResponse = ToolsListResponse
  { toolsListTools :: [ToolDefinition]
  } deriving (Show, Eq, Generic)

instance ToJSON ToolsListResponse where
  toJSON resp = object
    [ "tools" .= toolsListTools resp
    ]

-- | Tools call request
data ToolsCallRequest = ToolsCallRequest
  { toolsCallName      :: Text
  , toolsCallArguments :: Maybe (Map Text Value)
  } deriving (Show, Eq, Generic)

instance FromJSON ToolsCallRequest where
  parseJSON = withObject "ToolsCallRequest" $ \o -> ToolsCallRequest
    <$> o .: "name"
    <*> o .:? "arguments"

-- | Tools call response (2025-06-18 enhanced)
data ToolsCallResponse = ToolsCallResponse
  { toolsCallContent           :: [Content]
  , toolsCallIsError           :: Maybe Bool
  , toolsCallStructuredContent :: Maybe Value  -- ^ structuredContent (2025-06-18+)
  , toolsCallMeta              :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON ToolsCallResponse where
  toJSON resp = object $
    [ "content" .= toolsCallContent resp
    ] ++ maybe [] (\e -> ["isError" .= e]) (toolsCallIsError resp)
      ++ maybe [] (\s -> ["structuredContent" .= s]) (toolsCallStructuredContent resp)
      ++ maybe [] (\m -> ["_meta" .= m]) (toolsCallMeta resp)

-- | List changed notification
data ListChangedNotification = ListChangedNotification
  deriving (Show, Eq, Generic)

instance ToJSON ListChangedNotification where
  toJSON ListChangedNotification = object []
