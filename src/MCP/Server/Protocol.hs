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
  ) where

import           Data.Aeson
import           Data.Map         (Map)
import           Data.Maybe       (catMaybes)
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           MCP.Server.Types

protocolVersion :: Text
protocolVersion = "2025-06-18"


-- | Initialize request
data InitializeRequest = InitializeRequest
  { initProtocolVersion :: Text
  , initCapabilities    :: Value
  , initClientInfo      :: Maybe Value
  } deriving (Show, Eq, Generic)

instance FromJSON InitializeRequest where
  parseJSON = withObject "InitializeRequest" $ \o -> InitializeRequest
    <$> o .: "protocolVersion"
    <*> o .: "capabilities"
    <*> o .:? "clientInfo"

-- | Initialize response
data InitializeResponse = InitializeResponse
  { initRespProtocolVersion :: Text
  , initRespCapabilities    :: ServerCapabilities
  , initRespServerInfo      :: McpServerInfo
  , initRespMeta            :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON InitializeResponse where
  toJSON resp = object $ catMaybes
    [ Just ("protocolVersion" .= initRespProtocolVersion resp)
    , Just ("capabilities" .= initRespCapabilities resp)
    , Just ("serverInfo" .= object
        [ "name" .= serverName (initRespServerInfo resp)
        , "version" .= serverVersion (initRespServerInfo resp)
        , "instructions" .= serverInstructions (initRespServerInfo resp)
        ])
    , fmap ("_meta" .=) (initRespMeta resp)
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

-- | Prompts get response
data PromptsGetResponse = PromptsGetResponse
  { promptsGetDescription :: Maybe Text
  , promptsGetMessages    :: [PromptMessage]
  } deriving (Show, Eq, Generic)

instance ToJSON PromptsGetResponse where
  toJSON resp = object $
    [ "messages" .= promptsGetMessages resp
    ] ++ maybe [] (\d -> ["description" .= d]) (promptsGetDescription resp)

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

-- | Tools call response
data ToolsCallResponse = ToolsCallResponse
  { toolsCallContent :: [Content]
  , toolsCallIsError :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ToolsCallResponse where
  toJSON resp = object $
    [ "content" .= toolsCallContent resp
    ] ++ maybe [] (\e -> ["isError" .= e]) (toolsCallIsError resp)

-- | List changed notification
data ListChangedNotification = ListChangedNotification
  deriving (Show, Eq, Generic)

instance ToJSON ListChangedNotification where
  toJSON ListChangedNotification = object []
