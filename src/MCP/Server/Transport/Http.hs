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
import qualified Data.Aeson.KeyMap        as KM
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
import           MCP.Server.Protocol (protocolVersion, supportedVersions)
import           MCP.Server.Types

-- | HTTP transport configuration following the MCP Streamable HTTP specification
--
-- Note: 'HttpConfig' has no 'Show'/'Eq' instances because 'httpAuthorize' is a
-- function.
data HttpConfig = HttpConfig
  { httpPort      :: Int      -- ^ Port to listen on
  , httpHost      :: String   -- ^ Host to bind to (default "localhost")
  , httpEndpoint  :: String   -- ^ MCP endpoint path (default "/mcp")
  , httpVerbose   :: Bool     -- ^ Enable verbose logging (default False)
  , httpAuthorize :: Maybe (Maybe Text -> IO (Maybe Value))
      -- ^ Optional authorization callback. 'Nothing' disables authentication.
      --   When @'Just' check@, the bearer token presented by each request (or
      --   'Nothing' when absent / not a Bearer credential) is passed to
      --   @check@, which returns the caller's principal: @'Just' principal@
      --   authorizes the request — the principal (e.g. a role) is placed in the
      --   handler 'ClientContext' as 'clientPrincipal' — while 'Nothing' rejects
      --   the request with @401@. Validation and principal assignment are left
      --   entirely to the caller.
  }

-- | Default HTTP configuration (authentication disabled).
defaultHttpConfig :: HttpConfig
defaultHttpConfig = HttpConfig
  { httpPort = 3000
  , httpHost = "localhost"
  , httpEndpoint = "/mcp"
  , httpVerbose = False
  , httpAuthorize = Nothing
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

  -- Authenticate and obtain the caller's principal (if any) before anything else.
  decision <- case httpAuthorize config of
    Nothing    -> pure (Just Nothing)      -- auth disabled: allowed, no principal
    Just check -> fmap (fmap Just) (check (bearerToken req))
  case decision of
    Nothing -> do
      logVerbose config "Request rejected by authorization callback"
      respond $ Wai.responseLBS
        status401
        [("Content-Type", "application/json"), ("WWW-Authenticate", "Bearer")]
        (encode $ object ["error" .= ("Unauthorized" :: Text)])
    Just principal -> do
      let ctx = ClientContext { clientToken = bearerToken req, clientPrincipal = principal }
      -- Check if this is our MCP endpoint
      if TE.decodeUtf8 (Wai.rawPathInfo req) == T.pack (httpEndpoint config)
        then handleMcpRequest config serverInfo handlers ctx req respond
        else respond $ Wai.responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

-- | The bearer token presented by a request, if any: the value following
-- @Authorization: Bearer @.
bearerToken :: Wai.Request -> Maybe Text
bearerToken req =
  lookup hAuthorization (Wai.requestHeaders req)
    >>= T.stripPrefix "Bearer " . TE.decodeUtf8

-- | Handle MCP requests according to Streamable HTTP specification
handleMcpRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> ClientContext -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleMcpRequest config serverInfo handlers ctx req respond = do
  -- Read the POST body up front so we can identify the `initialize` request:
  -- it negotiates the protocol version in its *body*, so (per the Streamable
  -- HTTP spec, which scopes the MCP-Protocol-Version header to "subsequent
  -- requests") it is exempt from the header check. For any other request a
  -- *missing* header is accepted, while a *present but unsupported* one is
  -- rejected with 400.
  body <- if Wai.requestMethod req == "POST" then Wai.strictRequestBody req else pure ""
  if extractMethod body /= Just "initialize" && not (versionHeaderSupported req)
    then do
      logVerbose config "Request rejected: unsupported MCP-Protocol-Version header"
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Unsupported protocol version. Supported versions: " <> T.intercalate ", " supportedVersions)])
    else
        case Wai.requestMethod req of
          -- GET requests for endpoint discovery
          "GET" -> do
            let discoveryResponse = object
                  [ "name" .= serverName serverInfo
                  , "version" .= serverVersion serverInfo
                  , "description" .= serverInstructions serverInfo
                  , "protocolVersion" .= protocolVersion
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
            logVerbose config $ "Received POST body (" ++ show (BSL.length body) ++ " bytes): " ++ take 200 (show body)
            handleJsonRpcRequest config serverInfo handlers ctx body respond

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

-- | True unless the request carries a *present but unsupported*
-- MCP-Protocol-Version header. A missing header is treated as acceptable, since
-- the spec allows the server to assume a default protocol version in that case.
versionHeaderSupported :: Wai.Request -> Bool
versionHeaderSupported req =
  case lookup "MCP-Protocol-Version" (Wai.requestHeaders req) of
    Nothing -> True
    Just hv -> TE.decodeUtf8 hv `elem` supportedVersions

-- | Peek at a JSON-RPC message body to read its @method@ (if present).
extractMethod :: BSL.ByteString -> Maybe Text
extractMethod body = case decode body of
  Just (Object o) -> case KM.lookup "method" o of
    Just (String m) -> Just m
    _               -> Nothing
  _ -> Nothing

-- | Handle JSON-RPC request from HTTP body
handleJsonRpcRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> ClientContext -> BSL.ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleJsonRpcRequest config serverInfo handlers ctx body respond = do
  case eitherDecode body of
    Left err -> do
      hPutStrLn stderr $ "JSON parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid JSON" :: Text)])

    Right jsonValue -> handleSingleJsonRpc config serverInfo handlers ctx jsonValue respond

-- | Handle a single JSON-RPC message
handleSingleJsonRpc :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> ClientContext -> Value -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleSingleJsonRpc config serverInfo handlers ctx jsonValue respond = do
  case parseJsonRpcMessage jsonValue of
    Left err -> do
      hPutStrLn stderr $ "JSON-RPC parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid JSON-RPC" :: Text)])

    Right message -> do
      logVerbose config $ "Processing HTTP message: " ++ show (getMessageSummary message)
      maybeResponse <- handleMcpMessage serverInfo handlers ctx message

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
