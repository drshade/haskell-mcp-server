{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Handlers
  ( -- * Core Message Handling
    handleMcpMessage
  , jsonValueToText

    -- * Individual Request Handlers
  , handleInitialize
  , handlePing
  , handlePromptsList
  , handlePromptsGet
  , handleResourcesList
  , handleResourcesRead
  , handleToolsList
  , handleToolsCall

    -- * Protocol Support
  , validateProtocolVersion
  , getMessageSummary

    -- * Error Conversion
  , errorCodeFromMcpError
  , errorMessageFromMcpError
  ) where

import           Data.Aeson
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
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
validateProtocolVersion :: Text -> Either Text Text
validateProtocolVersion clientVersion
  | clientVersion `elem` supportedVersions = Right clientVersion  -- Supported: echo the client's own version
  | otherwise = Right protocolVersion  -- Unknown: negotiate down to the server's default version

-- | Handle an MCP message and return a response if needed
handleMcpMessage :: McpServerInfo
                 -> McpServerHandlers
                 -> ClientContext
                 -> JsonRpcMessage
                 -> IO (Maybe JsonRpcMessage)
handleMcpMessage serverInfo handlers ctx (JsonRpcMessageRequest req) = do
  response <- case requestMethod req of
    "initialize" -> handleInitialize serverInfo handlers req
    "ping" -> handlePing req
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
  return $ Just $ JsonRpcMessageResponse response

handleMcpMessage _ _ _ (JsonRpcMessageNotification notif) = do
  case notificationMethod notif of
    "notifications/initialized" ->
      hPutStrLn stderr "Received initialized notification - server is ready for operation"
    _ ->
      hPutStrLn stderr $ "Received unknown notification: " ++ T.unpack (notificationMethod notif)
  return Nothing

handleMcpMessage _ _ _ (JsonRpcMessageResponse _) =
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
              -- Only advertise a capability that actually has a handler.
              -- Advertising e.g. "prompts" while prompts/list returns an error
              -- makes strict clients (e.g. Crush) drop the whole server.
              let capabilities = ServerCapabilities
                    { capabilityPrompts   = PromptCapabilities { promptListChanged = Nothing } <$ prompts handlers
                    , capabilityResources = ResourceCapabilities { resourceSubscribe = Nothing, resourceListChanged = Nothing } <$ resources handlers
                    , capabilityTools     = ToolCapabilities { toolListChanged = Nothing } <$ tools handlers
                    , capabilityLogging   = Nothing  -- Not supported yet
                    }
              let response = InitializeResponse
                    { initRespProtocolVersion = negotiatedVersion
                    , initRespCapabilities = capabilities
                    , initRespServerInfo = serverInfo
                    }
              return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle ping request
handlePing :: JsonRpcRequest -> IO JsonRpcResponse
handlePing req = return $ makeSuccessResponse (requestId req) (toJSON PongResponse)

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
