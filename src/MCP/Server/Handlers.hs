{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Handlers
  ( -- * Core Message Handling
    handleMcpMessage
  , jsonValueToText

    -- * Individual Request Handlers
  , handleInitialize
  , handlePing
  , handleServerDiscover
  , handlePromptsList
  , handlePromptsGet
  , handleResourcesList
  , handleResourcesRead
  , handleToolsList
  , handleToolsCall

    -- * Protocol Support
  , validateProtocolVersion
  , getMessageSummary
  , metaProtocolVersion
  , unsupportedVersionError

    -- * Error Conversion
  , errorCodeFromMcpError
  , errorMessageFromMcpError
  ) where

import           Data.Aeson
import qualified Data.Aeson.Key         as Key
import qualified Data.Aeson.KeyMap      as KM
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           System.IO              (hPutStrLn, stderr)

import           MCP.Server.JsonRpc
import           MCP.Server.Protocol
import           MCP.Server.Types

-- | Convert JSON Value to Text representation suitable for handlers
jsonValueToText :: Value -> Text
jsonValueToText (String t) = t
jsonValueToText (Number n) =
    -- Check if it's a whole number, if so format as integer
    if fromInteger (round n) == n
        then T.pack $ show (round n :: Integer)
        else T.pack $ show n
jsonValueToText (Bool True) = "true"
jsonValueToText (Bool False) = "false"
jsonValueToText Null = ""
jsonValueToText v = T.pack $ show v

-- | Extract a brief summary of a JSON-RPC message for logging
getMessageSummary :: JsonRpcMessage -> String
getMessageSummary (JsonRpcMessageRequest req) =
  "Request[" ++ show (requestId req) ++ "] " ++ T.unpack (requestMethod req)
getMessageSummary (JsonRpcMessageNotification notif) =
  "Notification " ++ T.unpack (notificationMethod notif)
getMessageSummary (JsonRpcMessageResponse resp) =
  "Response[" ++ show (responseId resp) ++ "]"

-- | Validate protocol version and return negotiated version
-- Per MCP spec: "If the server supports the requested protocol version,
-- it MUST respond with the same version. Otherwise, the server MUST respond
-- with another protocol version it supports."
--
-- Only /legacy/ revisions are eligible here: a client that negotiates via
-- @initialize@ is legacy by definition, so proposing @2026-07-28@ (or
-- anything unknown) negotiates down to the newest legacy revision rather
-- than promising stateless semantics inside a handshake.
validateProtocolVersion :: Text -> Either Text Text
validateProtocolVersion clientVersion
  | clientVersion `elem` supportedVersions = Right clientVersion  -- Supported: echo the client's own version
  | otherwise = Right protocolVersion  -- Unknown: negotiate down to the server's default version

-- | Look up an @io.modelcontextprotocol/\<key\>@ entry in a request's
-- params @_meta@ object.
metaLookup :: Text -> Maybe Value -> Maybe Value
metaLookup key params = do
  Object o <- params
  Object m <- KM.lookup "_meta" o
  KM.lookup (Key.fromText ("io.modelcontextprotocol/" <> key)) m

-- | The protocol revision a request declares in its params @_meta@
-- (modern, 2026-07-28+ clients). 'Nothing' for legacy requests.
metaProtocolVersion :: Maybe Value -> Maybe Text
metaProtocolVersion params = case metaLookup "protocolVersion" params of
  Just (String v) -> Just v
  _               -> Nothing

-- | The @UnsupportedProtocolVersionError@ (-32022) for a declared version
-- this library does not implement, listing what it does.
unsupportedVersionError :: Text -> JsonRpcError
unsupportedVersionError requested = JsonRpcError
  { errorCode = -32022
  , errorMessage = "Unsupported protocol version"
  , errorData = Just $ object
      [ "supported" .= allVersions
      , "requested" .= requested
      ]
  }

-- | Methods whose modern results carry the required cacheability fields.
cacheableMethods :: [Text]
cacheableMethods =
  [ "server/discover"
  , "tools/list"
  , "prompts/list"
  , "resources/list"
  , "resources/read"
  ]

-- | Stamp the modern-revision result envelope onto a successful response:
-- @resultType: \"complete\"@, the server's identity in result @_meta@, and
-- (for cacheable methods) @ttlMs@ and @cacheScope@. Error responses and
-- legacy responses pass through untouched.
decorateModern :: McpServerInfo -> CacheHints -> Text -> JsonRpcResponse -> JsonRpcResponse
decorateModern serverInfo hints method resp = case responseResult resp of
  Just (Object o) -> resp { responseResult = Just $ Object $ decorate o }
  _               -> resp
  where
    decorate = insertCache . KM.insert "resultType" (String "complete") . insertMeta
    insertMeta o = KM.insert "_meta" (Object meta) o
      where meta = case KM.lookup "_meta" o of
              Just (Object m) -> KM.insert serverInfoKey serverInfoVal m
              _               -> KM.singleton serverInfoKey serverInfoVal
    serverInfoKey = "io.modelcontextprotocol/serverInfo"
    serverInfoVal = object
      [ "name" .= serverName serverInfo
      , "version" .= serverVersion serverInfo
      ]
    insertCache o
      | method `elem` cacheableMethods =
          KM.insert "ttlMs" (toJSON (cacheTtlMs hints)) $
          KM.insert "cacheScope"
            (String (if cacheScopePublic hints then "public" else "private")) o
      | otherwise = o

-- | Handle an MCP message and return a response if needed.
--
-- The server is dual-era: a request that declares a protocol revision in
-- its params @_meta@ (2026-07-28+) is served statelessly with the modern
-- result envelope; a request that does not is served exactly as before
-- under the revision negotiated by @initialize@.
handleMcpMessage :: McpServerInfo
                 -> CacheHints
                 -> McpServerHandlers
                 -> ClientContext
                 -> JsonRpcMessage
                 -> IO (Maybe JsonRpcMessage)
handleMcpMessage serverInfo hints handlers ctx0 (JsonRpcMessageRequest req) = do
  let params = requestParams req
      declaredVersion = metaProtocolVersion params
  case declaredVersion of
    Just v | v `notElem` modernVersions ->
      return $ Just $ JsonRpcMessageResponse $
        makeErrorResponse (requestId req) (unsupportedVersionError v)
    _ -> do
      -- server/discover is itself a modern-revision method (and the
      -- backwards-compatibility probe), so it always gets the modern
      -- envelope even if a probing client omitted _meta.
      let modern = isJust declaredVersion || requestMethod req == "server/discover"
          ctx = ctx0
            { clientProtocolVersion = declaredVersion
            , clientInfo = metaLookup "clientInfo" params
            , clientCapabilities = metaLookup "clientCapabilities" params
            }
      response <- case requestMethod req of
        -- Era purity: the modern revision has neither initialize (nothing to
        -- negotiate statelessly) nor ping (removed) — a request declaring a
        -- modern revision must be served "according to this revision", so
        -- these are unknown methods there.
        m | isJust declaredVersion, m `elem` ["initialize", "ping"] ->
              return $ makeErrorResponse (requestId req) $ JsonRpcError
                { errorCode = -32601
                , errorMessage = "Method not found: " <> m
                    <> " (not part of protocol revision " <> fromMaybe "" declaredVersion <> ")"
                , errorData = Nothing
                }
        "initialize" -> handleInitialize serverInfo handlers req
        "ping" -> handlePing req
        "server/discover" -> handleServerDiscover serverInfo handlers req
        "prompts/list" -> handlePromptsList handlers ctx req
        "prompts/get" -> handlePromptsGet handlers ctx req
        "resources/list" -> handleResourcesList handlers ctx req
        "resources/read" -> handleResourcesRead handlers ctx req
        "tools/list" -> handleToolsList handlers ctx req
        "tools/call" -> handleToolsCall handlers ctx req
        method -> return $ makeErrorResponse (requestId req) $ JsonRpcError
          { errorCode = -32601
          , errorMessage = "Method not found: " <> method
          , errorData = Nothing
          }
      let response' = if modern
            then decorateModern serverInfo hints (requestMethod req) response
            else response
      return $ Just $ JsonRpcMessageResponse response'

handleMcpMessage _ _ _ _ (JsonRpcMessageNotification notif) = do
  case notificationMethod notif of
    "notifications/initialized" ->
      hPutStrLn stderr "Received initialized notification - server is ready for operation"
    _ ->
      hPutStrLn stderr $ "Received unknown notification: " ++ T.unpack (notificationMethod notif)
  return Nothing

handleMcpMessage _ _ _ _ (JsonRpcMessageResponse _) =
  return Nothing

-- | Handle initialize request
handleInitialize :: McpServerInfo -> McpServerHandlers -> JsonRpcRequest -> IO JsonRpcResponse
handleInitialize serverInfo handlers req = do
  case requestParams req of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32602
      , errorMessage = "Missing required parameters for initialize"
      , errorData = Nothing
      }
    Just params ->
      case fromJSON params of
        Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
          { errorCode = -32602
          , errorMessage = "Invalid initialize parameters: " <> T.pack err
          , errorData = Nothing
          }
        Success initReq -> do
          -- Check protocol version compatibility
          let clientVersion = initProtocolVersion initReq
          case validateProtocolVersion clientVersion of
            Left errorMsg -> return $ makeErrorResponse (requestId req) $ JsonRpcError
              { errorCode = -32602
              , errorMessage = errorMsg
              , errorData = Nothing
              }
            Right negotiatedVersion -> do
              hPutStrLn stderr $ "Client version: " ++ T.unpack clientVersion ++ ", using: " ++ T.unpack negotiatedVersion
              let response = InitializeResponse
                    { initRespProtocolVersion = negotiatedVersion
                    , initRespCapabilities = handlerCapabilities handlers
                    , initRespServerInfo = serverInfo
                    }
              return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Only advertise a capability that actually has a handler.
-- Advertising e.g. "prompts" while prompts/list returns an error
-- makes strict clients (e.g. Crush) drop the whole server.
handlerCapabilities :: McpServerHandlers -> ServerCapabilities
handlerCapabilities handlers = ServerCapabilities
  { capabilityPrompts   = PromptCapabilities { promptListChanged = Nothing } <$ prompts handlers
  , capabilityResources = ResourceCapabilities { resourceSubscribe = Nothing, resourceListChanged = Nothing } <$ resources handlers
  , capabilityTools     = ToolCapabilities { toolListChanged = Nothing } <$ tools handlers
  , capabilityLogging   = Nothing  -- Not supported yet
  }

-- | Handle ping request
handlePing :: JsonRpcRequest -> IO JsonRpcResponse
handlePing req = return $ makeSuccessResponse (requestId req) (toJSON PongResponse)

-- | Handle server/discover (2026-07-28+): the server's supported protocol
-- revisions, capabilities and identity, available before any other request.
-- Also serves as the stdio backwards-compatibility probe. The modern result
-- envelope (resultType, serverInfo _meta, cacheability) is stamped on by
-- 'decorateModern'.
handleServerDiscover :: McpServerInfo -> McpServerHandlers -> JsonRpcRequest -> IO JsonRpcResponse
handleServerDiscover serverInfo handlers req = do
  let result = object $
        [ "supportedVersions" .= allVersions
        , "capabilities" .= handlerCapabilities handlers
        ] ++ (if T.null (serverInstructions serverInfo)
                then []
                else ["instructions" .= serverInstructions serverInfo])
  return $ makeSuccessResponse (requestId req) result

-- | Handle prompts/list request
handlePromptsList :: McpServerHandlers -> ClientContext -> JsonRpcRequest -> IO JsonRpcResponse
handlePromptsList handlers ctx req =
  case prompts handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Prompts not supported"
      , errorData = Nothing
      }
    Just (listHandler, _) -> do
      promptsList <- listHandler ctx
      let response = PromptsListResponse
            { promptsListPrompts = promptsList
            }
      return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle prompts/get request
handlePromptsGet :: McpServerHandlers -> ClientContext -> JsonRpcRequest -> IO JsonRpcResponse
handlePromptsGet handlers ctx req =
  case prompts handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Prompts not supported"
      , errorData = Nothing
      }
    Just (_, getHandler) -> do
      case requestParams req of
        Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
          { errorCode = -32602
          , errorMessage = "Missing parameters"
          , errorData = Nothing
          }
        Just params ->
          case fromJSON params of
            Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
              { errorCode = -32602
              , errorMessage = "Invalid parameters: " <> T.pack err
              , errorData = Nothing
              }
            Success getReq -> do
              -- Prompt arguments are string-valued per the MCP spec; flatten
              -- any non-string values a lenient client may have sent.
              let args = maybe Map.empty (fmap jsonValueToText) (promptsGetArguments getReq)
              result <- getHandler ctx (promptsGetName getReq) args
              case result of
                Left err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
                  { errorCode = errorCodeFromMcpError err
                  , errorMessage = errorMessageFromMcpError err
                  , errorData = Nothing
                  }
                Right promptRes -> do
                  let response = PromptsGetResponse
                        { promptsGetDescription = promptResultDescription promptRes
                        , promptsGetMessages = promptResultMessages promptRes
                        , promptsGetMeta = Nothing
                        }
                  return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle resources/list request
handleResourcesList :: McpServerHandlers -> ClientContext -> JsonRpcRequest -> IO JsonRpcResponse
handleResourcesList handlers ctx req =
  case resources handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Resources not supported"
      , errorData = Nothing
      }
    Just (listHandler, _) -> do
      resourcesList <- listHandler ctx
      let response = ResourcesListResponse
            { resourcesListResources = resourcesList
            }
      return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle resources/read request
handleResourcesRead :: McpServerHandlers -> ClientContext -> JsonRpcRequest -> IO JsonRpcResponse
handleResourcesRead handlers ctx req =
  case resources handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Resources not supported"
      , errorData = Nothing
      }
    Just (_, readHandler) -> do
      case requestParams req of
        Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
          { errorCode = -32602
          , errorMessage = "Missing parameters"
          , errorData = Nothing
          }
        Just params ->
          case fromJSON params of
            Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
              { errorCode = -32602
              , errorMessage = "Invalid parameters: " <> T.pack err
              , errorData = Nothing
              }
            Success readReq -> do
              result <- readHandler ctx (resourcesReadUri readReq)
              case result of
                Left err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
                  { errorCode = errorCodeFromMcpError err
                  , errorMessage = errorMessageFromMcpError err
                  , errorData = Nothing
                  }
                Right resourceContent -> do
                  let response = ResourcesReadResponse
                        { resourcesReadContents = [resourceContent]
                        }
                  return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle tools/list request
handleToolsList :: McpServerHandlers -> ClientContext -> JsonRpcRequest -> IO JsonRpcResponse
handleToolsList handlers ctx req =
  case tools handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Tools not supported"
      , errorData = Nothing
      }
    Just (listHandler, _) -> do
      toolsList <- listHandler ctx
      let response = ToolsListResponse
            { toolsListTools = toolsList
            }
      return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle tools/call request
handleToolsCall :: McpServerHandlers -> ClientContext -> JsonRpcRequest -> IO JsonRpcResponse
handleToolsCall handlers ctx req =
  case tools handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Tools not supported"
      , errorData = Nothing
      }
    Just (_, callHandler) -> do
      case requestParams req of
        Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
          { errorCode = -32602
          , errorMessage = "Missing parameters"
          , errorData = Nothing
          }
        Just params ->
          case fromJSON params of
            Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
              { errorCode = -32602
              , errorMessage = "Invalid parameters: " <> T.pack err
              , errorData = Nothing
              }
            Success callReq -> do
              -- Tool arguments are passed through as full JSON values.
              let args = fromMaybe Map.empty (toolsCallArguments callReq)
              result <- callHandler ctx (toolsCallName callReq) args
              case result of
                Left err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
                  { errorCode = errorCodeFromMcpError err
                  , errorMessage = errorMessageFromMcpError err
                  , errorData = Nothing
                  }
                Right toolRes -> do
                  let response = ToolsCallResponse
                        { toolsCallContent = toolResultContent toolRes
                        , toolsCallIsError = if toolResultIsError toolRes then Just True else Nothing
                        , toolsCallStructuredContent = toolResultStructured toolRes
                        , toolsCallMeta = toolResultMeta toolRes
                        }
                  return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Convert MCP error to JSON-RPC error code
errorCodeFromMcpError :: Error -> Int
errorCodeFromMcpError (InvalidPromptName _)     = -32602
errorCodeFromMcpError (MissingRequiredParams _) = -32602
errorCodeFromMcpError (ResourceNotFound _)      = -32602
errorCodeFromMcpError (InternalError _)         = -32603
errorCodeFromMcpError (UnknownTool _)           = -32602
errorCodeFromMcpError (InvalidRequest _)        = -32600
errorCodeFromMcpError (MethodNotFound _)        = -32601
errorCodeFromMcpError (InvalidParams _)         = -32602

-- | Convert MCP error to JSON-RPC error message
errorMessageFromMcpError :: Error -> Text
errorMessageFromMcpError (InvalidPromptName msg) = "Invalid prompt name: " <> msg
errorMessageFromMcpError (MissingRequiredParams msg) = "Missing required parameters: " <> msg
errorMessageFromMcpError (ResourceNotFound msg) = "Resource not found: " <> msg
errorMessageFromMcpError (InternalError msg) = "Internal error: " <> msg
errorMessageFromMcpError (UnknownTool msg) = "Unknown tool: " <> msg
errorMessageFromMcpError (InvalidRequest msg) = "Invalid request: " <> msg
errorMessageFromMcpError (MethodNotFound msg) = "Method not found: " <> msg
errorMessageFromMcpError (InvalidParams msg) = "Invalid parameters: " <> msg
