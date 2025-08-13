{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Http
  ( -- * HTTP Transport
    HttpConfig(..)
  , transportRunHttp
  , defaultHttpConfig
  ) where

import           Control.Monad            (when)
import           Data.Aeson
import qualified Data.ByteString.Lazy     as BSL
import           Data.String              (IsString (fromString))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Network.HTTP.Types
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.IO                (hPutStrLn, stderr)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
import           MCP.Server.Types

-- | HTTP transport configuration following MCP 2025-06-18 Streamable HTTP specification
data HttpConfig = HttpConfig
  { httpPort     :: Int      -- ^ Port to listen on
  , httpHost     :: String   -- ^ Host to bind to (default "localhost")
  , httpEndpoint :: String   -- ^ MCP endpoint path (default "/mcp")
  , httpVerbose  :: Bool     -- ^ Enable verbose logging (default False)
  } deriving (Show, Eq)

-- | Default HTTP configuration
defaultHttpConfig :: HttpConfig
defaultHttpConfig = HttpConfig
  { httpPort = 3000
  , httpHost = "localhost"
  , httpEndpoint = "/mcp"
  , httpVerbose = False
  }

-- | Helper for conditional logging
logVerbose :: HttpConfig -> String -> IO ()
logVerbose config msg = when (httpVerbose config) $ hPutStrLn stderr msg


-- | Transport-specific implementation for HTTP
transportRunHttp :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> IO ()
transportRunHttp config serverInfo handlers = do
  let settings = Warp.setHost (fromString $ httpHost config) $
                 Warp.setPort (httpPort config) $
                 Warp.defaultSettings

  putStrLn $ "Starting MCP HTTP server on " ++ httpHost config ++ ":" ++ show (httpPort config) ++ httpEndpoint config
  Warp.runSettings settings (mcpApplication config serverInfo handlers)

-- | WAI Application for MCP over HTTP
mcpApplication :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Wai.Application
mcpApplication config serverInfo handlers req respond = do
  -- Log the request
  logVerbose config $ "HTTP " ++ show (Wai.requestMethod req) ++ " " ++ T.unpack (TE.decodeUtf8 $ Wai.rawPathInfo req)

  -- Check if this is our MCP endpoint
  if TE.decodeUtf8 (Wai.rawPathInfo req) == T.pack (httpEndpoint config)
    then handleMcpRequest config serverInfo handlers req respond
    else respond $ Wai.responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

-- | Handle MCP requests according to Streamable HTTP specification
handleMcpRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleMcpRequest config serverInfo handlers req respond = do
  -- Check for mandatory MCP-Protocol-Version header (2025-06-18 requirement)
  case lookup "MCP-Protocol-Version" (Wai.requestHeaders req) of
    Nothing -> do
      logVerbose config "Request rejected: Missing MCP-Protocol-Version header"
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Missing required MCP-Protocol-Version header" :: Text)])
    Just headerValue -> 
      if TE.decodeUtf8 headerValue /= "2025-06-18" then do
        logVerbose config $ "Request rejected: Invalid protocol version: " ++ show headerValue
        respond $ Wai.responseLBS
          status400
          [("Content-Type", "application/json")]
          (encode $ object ["error" .= ("Unsupported protocol version. Server only supports 2025-06-18" :: Text)])
      else
        case Wai.requestMethod req of
          -- GET requests for endpoint discovery
          "GET" -> do
            let discoveryResponse = object
                  [ "name" .= serverName serverInfo
                  , "version" .= serverVersion serverInfo
                  , "description" .= serverInstructions serverInfo
                  , "protocolVersion" .= ("2025-06-18" :: Text)
                  , "capabilities" .= object
                      [ "tools" .= object []
                      , "prompts" .= object []
                      , "resources" .= object []
                      ]
                  ]
            logVerbose config $ "Sending server discovery response: " ++ show discoveryResponse
            respond $ Wai.responseLBS
              status200
              [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")]
              (encode discoveryResponse)

          -- POST requests for JSON-RPC messages
          "POST" -> do
            -- Read request body
            body <- Wai.strictRequestBody req
            logVerbose config $ "Received POST body (" ++ show (BSL.length body) ++ " bytes): " ++ take 200 (show body)
            handleJsonRpcRequest config serverInfo handlers body respond

          -- OPTIONS for CORS preflight
          "OPTIONS" -> respond $ Wai.responseLBS
            status200
            [ ("Access-Control-Allow-Origin", "*")
            , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
            , ("Access-Control-Allow-Headers", "Content-Type, MCP-Protocol-Version")
            ]
            ""

          -- Unsupported methods
          _ -> respond $ Wai.responseLBS
            status405
            [("Content-Type", "text/plain"), ("Allow", "GET, POST, OPTIONS")]
            "Method Not Allowed"

-- | Handle JSON-RPC request from HTTP body
handleJsonRpcRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> BSL.ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleJsonRpcRequest config serverInfo handlers body respond = do
  case eitherDecode body of
    Left err -> do
      hPutStrLn stderr $ "JSON parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid JSON" :: Text)])

    Right jsonValue -> handleSingleJsonRpc config serverInfo handlers jsonValue respond

-- | Handle a single JSON-RPC message
handleSingleJsonRpc :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Value -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleSingleJsonRpc config serverInfo handlers jsonValue respond = do
  case parseJsonRpcMessage jsonValue of
    Left err -> do
      hPutStrLn stderr $ "JSON-RPC parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid JSON-RPC" :: Text)])

    Right message -> do
      logVerbose config $ "Processing HTTP message: " ++ show (getMessageSummary message)
      maybeResponse <- handleMcpMessage serverInfo handlers message

      case maybeResponse of
        Just responseMsg -> do
          let responseJson = encode $ encodeJsonRpcMessage responseMsg
          logVerbose config $ "Sending HTTP response for: " ++ show (getMessageSummary message)
          respond $ Wai.responseLBS
            status200
            [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")]
            responseJson

        Nothing -> do
          logVerbose config $ "No response needed for: " ++ show (getMessageSummary message)
          -- For notifications, return 200 with empty JSON object (per MCP spec)
          respond $ Wai.responseLBS 
            status200 
            [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")] 
            "{}"


