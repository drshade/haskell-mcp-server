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

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.Map               as Map
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
  | clientVersion == protocolVersion = Right protocolVersion  -- Exact match
  | otherwise = Right protocolVersion  -- Negotiate: return server's supported version

-- | Handle an MCP message and return a response if needed
handleMcpMessage :: (MonadIO m)
                 => McpServerInfo
                 -> McpServerHandlers m
                 -> JsonRpcMessage
                 -> m (Maybe JsonRpcMessage)
handleMcpMessage serverInfo handlers (JsonRpcMessageRequest req) = do
  response <- case requestMethod req of
    "initialize" -> handleInitialize serverInfo req
    "ping" -> handlePing req
    "prompts/list" -> handlePromptsList handlers req
    "prompts/get" -> handlePromptsGet handlers req
    "resources/list" -> handleResourcesList handlers req
    "resources/read" -> handleResourcesRead handlers req
    "tools/list" -> handleToolsList handlers req
    "tools/call" -> handleToolsCall handlers req
    method -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Method not found: " <> method
      , errorData = Nothing
      }
  return $ Just $ JsonRpcMessageResponse response

handleMcpMessage _ _ (JsonRpcMessageNotification notif) = do
  case notificationMethod notif of
    "notifications/initialized" -> do
      liftIO $ hPutStrLn stderr "Received initialized notification - server is ready for operation"
      return ()
    _ -> do
      liftIO $ hPutStrLn stderr $ "Received unknown notification: " ++ T.unpack (notificationMethod notif)
      return ()
  return Nothing

handleMcpMessage _ _ (JsonRpcMessageResponse _) =
  return Nothing

-- | Handle initialize request
handleInitialize :: (MonadIO m) => McpServerInfo -> JsonRpcRequest -> m JsonRpcResponse
handleInitialize serverInfo req = do
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
              liftIO $ hPutStrLn stderr $ "Client version: " ++ T.unpack clientVersion ++ ", using: " ++ T.unpack negotiatedVersion
              let capabilities = ServerCapabilities
                    { capabilityPrompts = Just $ PromptCapabilities { promptListChanged = Nothing }
                    , capabilityResources = Just $ ResourceCapabilities { resourceSubscribe = Nothing, resourceListChanged = Nothing }
                    , capabilityTools = Just $ ToolCapabilities { toolListChanged = Nothing }
                    , capabilityLogging = Nothing  -- Not supported yet
                    }
              let response = InitializeResponse
                    { initRespProtocolVersion = negotiatedVersion
                    , initRespCapabilities = capabilities
                    , initRespServerInfo = serverInfo
                    }
              return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle ping request
handlePing :: (MonadIO m) => JsonRpcRequest -> m JsonRpcResponse
handlePing req = return $ makeSuccessResponse (requestId req) (toJSON PongResponse)

-- | Handle prompts/list request
handlePromptsList :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handlePromptsList handlers req =
  case prompts handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Prompts not supported"
      , errorData = Nothing
      }
    Just (listHandler, _) -> do
      promptsList <- listHandler
      let response = PromptsListResponse
            { promptsListPrompts = promptsList
            }
      return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle prompts/get request
handlePromptsGet :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handlePromptsGet handlers req =
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
              let args = maybe [] (map (\(k, v) -> (k, jsonValueToText v)) . Map.toList) (promptsGetArguments getReq)
              result <- getHandler (promptsGetName getReq) args
              case result of
                Left err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
                  { errorCode = errorCodeFromMcpError err
                  , errorMessage = errorMessageFromMcpError err
                  , errorData = Nothing
                  }
                Right content -> do
                  let response = PromptsGetResponse
                        { promptsGetDescription = Nothing
                        , promptsGetMessages = [PromptMessage RoleUser content]
                        , promptsGetMeta = Nothing  -- Can be extended for additional metadata
                        }
                  return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle resources/list request
handleResourcesList :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handleResourcesList handlers req =
  case resources handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Resources not supported"
      , errorData = Nothing
      }
    Just (listHandler, _) -> do
      resourcesList <- listHandler
      let response = ResourcesListResponse
            { resourcesListResources = resourcesList
            }
      return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle resources/read request
handleResourcesRead :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handleResourcesRead handlers req =
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
              result <- readHandler (resourcesReadUri readReq)
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
handleToolsList :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handleToolsList handlers req =
  case tools handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Tools not supported"
      , errorData = Nothing
      }
    Just (listHandler, _) -> do
      toolsList <- listHandler
      let response = ToolsListResponse
            { toolsListTools = toolsList
            }
      return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle tools/call request
handleToolsCall :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handleToolsCall handlers req =
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
              let args = maybe [] (map (\(k, v) -> (k, jsonValueToText v)) . Map.toList) (toolsCallArguments callReq)
              result <- callHandler (toolsCallName callReq) args
              case result of
                Left err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
                  { errorCode = errorCodeFromMcpError err
                  , errorMessage = errorMessageFromMcpError err
                  , errorData = Nothing
                  }
                Right content -> do
                  let response = ToolsCallResponse
                        { toolsCallContent = [content]
                        , toolsCallIsError = Nothing
                        , toolsCallMeta = Nothing  -- Can be extended for structured output
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