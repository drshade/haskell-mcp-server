{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Http
  ( -- * HTTP Transport
    HttpConfig(..)
  , transportRunHttp
  , defaultHttpConfig
  , mcpApplication

    -- * Request validation (exposed for testing)
  , BodyPeek(..)
  , peekBody
  , peekIsModern
  , wantsStreamingResponse
  , validateRequestHeaders
  , decodeSentinel
  ) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Concurrent.MVar  (newMVar, withMVar)
import           Control.Concurrent.STM   (atomically, check, orElse,
                                           readTChan, readTVar, registerDelay)
import           Control.Exception        (uninterruptibleMask_)
import           Control.Monad            (forever, when)
import           Data.Aeson
import qualified Data.Aeson.KeyMap        as KM
import           Data.Aeson.Types         (parseEither)
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Builder  as B
import qualified Data.ByteString.Lazy     as BSL
import           Data.Maybe               (isJust)
import           Data.String              (IsString (fromString))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Network.HTTP.Types
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.IO                (hPutStrLn, stderr)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
import           MCP.Server.Notifications
import           MCP.Server.Protocol (allVersions, modernVersions,
                                      supportedVersions)
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
  , httpAllowedOrigins :: Maybe [Text]
      -- ^ Origin-validation policy (DNS-rebinding protection; the MCP
      --   Streamable HTTP spec requires servers to validate the @Origin@
      --   header). @'Just' origins@ rejects any request whose @Origin@ header
      --   is present but not in the list with @403@, and echoes the allowed
      --   origin in CORS headers instead of @*@. Requests without an @Origin@
      --   header (non-browser clients) are always allowed. 'Nothing' disables
      --   the check — only appropriate when the server is not reachable from
      --   a browser (e.g. bound to localhost for CLI clients).
  , httpCacheHints :: CacheHints
      -- ^ Cacheability hints stamped onto modern (2026-07-28+) list/read
      --   results.
  , httpNotifications :: Maybe NotificationSource
      -- ^ When configured, @subscriptions\/listen@ (2026-07-28) is served as
      --   a long-lived SSE stream delivering the change notifications the
      --   client opted into, and the @listChanged@\/@subscribe@ capabilities
      --   are advertised to modern clients. Legacy HTTP clients have no
      --   delivery channel (the deprecated GET SSE stream is not offered),
      --   so legacy @initialize@ does not advertise them.
  }

-- | Default HTTP configuration (authentication disabled).
defaultHttpConfig :: HttpConfig
defaultHttpConfig = HttpConfig
  { httpPort = 3000
  , httpHost = "localhost"
  , httpEndpoint = "/mcp"
  , httpVerbose = False
  , httpAuthorize = Nothing
  , httpAllowedOrigins = Nothing
  , httpCacheHints = defaultCacheHints
  , httpNotifications = Nothing
  }

-- | Helper for conditional logging
logVerbose :: HttpConfig -> String -> IO ()
logVerbose config msg = when (httpVerbose config) $ hPutStrLn stderr msg

-- | Whether a notification source is configured.
httpHasNotifications :: HttpConfig -> Bool
httpHasNotifications = isJust . httpNotifications

-- | What notification delivery this HTTP server offers.
httpNotifSupport :: HttpConfig -> NotificationSupport
httpNotifSupport config = NotificationSupport
  { supportsLegacyPush = False  -- no legacy HTTP delivery channel
  , supportsListen = httpHasNotifications config
  }

-- | Response headers for an SSE stream: no caching, and no proxy
-- buffering, so events reach the client immediately.
sseHeaders :: ResponseHeaders
sseHeaders =
  [ ("Content-Type", "text/event-stream")
  , ("Cache-Control", "no-cache")
  , ("X-Accel-Buffering", "no")
  ]


-- | Transport-specific implementation for HTTP
--
-- A client closing the connection is its cancellation signal: for SSE
-- responses the handler task is cancelled once the disconnect surfaces (at
-- most one keep-alive interval later), and for single-JSON responses Warp
-- tears the request thread down. Cancellation reaches handler code as an
-- asynchronous exception: handlers that acquire resources should release
-- them with 'Control.Exception.bracket'.
transportRunHttp :: HttpConfig -> McpServerInfo -> McpServerHandlers -> IO ()
transportRunHttp config serverInfo handlers = do
  let settings = Warp.setHost (fromString $ httpHost config) $
                 Warp.setPort (httpPort config) $
                 Warp.defaultSettings

  putStrLn $ "Starting MCP HTTP server on " ++ httpHost config ++ ":" ++ show (httpPort config) ++ httpEndpoint config
  Warp.runSettings settings (mcpApplication config serverInfo handlers)

-- | The MCP endpoint as a plain WAI 'Wai.Application', for embedding into
-- an existing WAI stack (your own Warp settings, TLS, middleware, or a
-- larger router) instead of letting 'transportRunHttp' run its own server.
--
-- When embedding, 'httpPort' and 'httpHost' are ignored — they only
-- configure the server 'transportRunHttp' starts. Everything else applies
-- as usual: requests are served only on the 'httpEndpoint' path (any other
-- path gets 404, so mount accordingly or match the path in your router),
-- Origin validation and bearer authentication run per 'httpAllowedOrigins'
-- and 'httpAuthorize', and @subscriptions\/listen@ streams work as long as
-- the surrounding stack does not buffer streaming responses.
mcpApplication :: HttpConfig -> McpServerInfo -> McpServerHandlers -> Wai.Application
mcpApplication config serverInfo handlers req respond0 = do
  -- Log the request
  logVerbose config $ "HTTP " ++ show (Wai.requestMethod req) ++ " " ++ T.unpack (TE.decodeUtf8 $ Wai.rawPathInfo req)

  let origin = lookup hOrigin (Wai.requestHeaders req)
      -- Every response carries exactly one Access-Control-Allow-Origin header:
      -- the validated request origin when a policy is configured, "*"
      -- otherwise. When the value depends on the request origin, Vary: Origin
      -- keeps shared caches from serving one origin's response to another.
      corsHeaders = case (httpAllowedOrigins config, origin) of
        (Just _, Just o) -> [("Access-Control-Allow-Origin", o), ("Vary", "Origin")]
        _                -> [("Access-Control-Allow-Origin", "*")]
      addCors hs = corsHeaders
                 ++ filter ((/= "Access-Control-Allow-Origin") . fst) hs
      respond = respond0 . Wai.mapResponseHeaders addCors

  -- Origin validation first (DNS-rebinding protection, a MUST in the spec).
  if not (originAllowed config req)
    then do
      logVerbose config $ "Request rejected: disallowed Origin " ++ show origin
      respond0 $ Wai.responseLBS
        status403
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Origin not allowed" :: Text)])
    else do
      -- Authenticate and obtain the caller's principal (if any) before anything
      -- else. CORS preflight requests are exempt: browsers never attach
      -- credentials to an OPTIONS preflight, and the preflight response is what
      -- tells the browser it may send the Authorization header at all.
      decision <- case httpAuthorize config of
        _ | Wai.requestMethod req == "OPTIONS"
                   -> pure (Just Nothing)      -- CORS preflight: no credentials
        Nothing    -> pure (Just Nothing)      -- auth disabled: allowed, no principal
        Just authorize -> fmap (fmap Just) (authorize (bearerToken req))
      case decision of
        Nothing -> do
          logVerbose config "Request rejected by authorization callback"
          respond $ Wai.responseLBS
            status401
            [("Content-Type", "application/json"), ("WWW-Authenticate", "Bearer")]
            (encode $ object ["error" .= ("Unauthorized" :: Text)])
        Just principal -> do
          let ctx = anonymousContext { clientToken = bearerToken req, clientPrincipal = principal }
          -- Check if this is our MCP endpoint
          if TE.decodeUtf8 (Wai.rawPathInfo req) == T.pack (httpEndpoint config)
            then handleMcpRequest config serverInfo handlers ctx req respond
            else respond $ Wai.responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

-- | Whether the request passes the configured Origin policy: with no policy
-- everything passes; with a policy, a present Origin header must be in the
-- allow-list (absent Origin means a non-browser client and always passes).
originAllowed :: HttpConfig -> Wai.Request -> Bool
originAllowed config req = case httpAllowedOrigins config of
  Nothing      -> True
  Just allowed -> case lookup hOrigin (Wai.requestHeaders req) of
    Nothing -> True
    Just o  -> TE.decodeUtf8With lenientDecode o `elem` allowed

-- | The bearer token presented by a request, if any: the value following
-- @Authorization: Bearer @. The scheme is matched case-insensitively per
-- RFC 7235, and invalid UTF-8 in the header is replaced rather than thrown.
bearerToken :: Wai.Request -> Maybe Text
bearerToken req = do
  header <- lookup hAuthorization (Wai.requestHeaders req)
  let (scheme, rest) = T.break (== ' ') (TE.decodeUtf8With lenientDecode header)
  if T.toCaseFold scheme == "bearer" && not (T.null rest)
    then Just (T.stripStart rest)
    else Nothing

-- | Handle MCP requests according to Streamable HTTP specification
handleMcpRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers -> ClientContext -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleMcpRequest config serverInfo handlers ctx req respond = do
  -- Read the POST body up front: the request era and header validation both
  -- depend on it. A body declaring a modern protocol revision in _meta gets
  -- the 2026-07-28 header validation (header/body match, Mcp-Method,
  -- Mcp-Name); a legacy body keeps the relaxed pre-2026 rules, where
  -- `initialize` is exempt (it negotiates in the body), a missing
  -- MCP-Protocol-Version header is accepted, and a present-but-unsupported
  -- one is rejected with 400.
  body <- if Wai.requestMethod req == "POST" then Wai.strictRequestBody req else pure ""
  let peek = peekBody body
      modern = peekIsModern peek
  case Wai.requestMethod req of
    -- POST requests for JSON-RPC messages
    "POST" ->
      case validateRequestHeaders req peek of
        Just err -> do
          logVerbose config $ "Request rejected: " ++ T.unpack (errorMessage err)
          respond $ Wai.responseLBS
            status400
            [("Content-Type", "application/json")]
            (encode $ makeErrorResponse (peekId peek) err)
        Nothing
          -- subscriptions/listen is transport-level: the response is a
          -- long-lived SSE stream, not a single JSON object
          | peekMethod peek == Just "subscriptions/listen"
          , Just src <- httpNotifications config
          -> handleSubscriptionsListen config src body respond
          -- Requests that opted into request-scoped notifications get an
          -- SSE response stream carrying them before the final response —
          -- but only for dispatchable request methods, so unknown methods
          -- keep their 404 and notification bodies keep their 202
          | wantsStreamingResponse peek
          -> handleStreamingRequest config serverInfo handlers ctx body respond
          | otherwise -> do
              logVerbose config $ "Received POST body (" ++ show (BSL.length body) ++ " bytes): " ++ take 200 (show body)
              handleJsonRpcRequest config serverInfo handlers ctx modern body respond

    -- OPTIONS for CORS preflight
    "OPTIONS" -> respond $ Wai.responseLBS
      status200
      [ ("Access-Control-Allow-Methods", "POST, OPTIONS")
      , ("Access-Control-Allow-Headers", "Content-Type, Authorization, MCP-Protocol-Version, Mcp-Method, Mcp-Name")
      ]
      ""

    -- Everything else, including GET: the MCP endpoint only speaks
    -- JSON-RPC over POST (this library does not offer server-initiated
    -- SSE streams, and revision 2026-07-28 requires 405 for GET).
    _ -> respond $ Wai.responseLBS
      status405
      [("Content-Type", "text/plain"), ("Allow", "POST, OPTIONS")]
      "Method Not Allowed"

-- | The parts of a JSON-RPC body that header validation and response-mode
-- selection need.
data BodyPeek = BodyPeek
  { peekMethod      :: Maybe Text
  , peekMetaVersion :: Maybe Text
  , peekName        :: Maybe Text  -- ^ params.name or params.uri
  , peekId          :: RequestId
  , peekHasId       :: Bool
      -- ^ Whether an @id@ member is present at all ('peekId' alone
      -- conflates an absent id — a notification — with an explicit null)
  , peekWantsStream :: Bool
      -- ^ The request opted into request-scoped notifications
      -- (@_meta.progressToken@ or @_meta.\"io.modelcontextprotocol/logLevel\"@),
      -- so the response must be an SSE stream that can carry them
  }

-- | Methods that run user handlers and may therefore emit request-scoped
-- notifications. Only these upgrade to an SSE response stream: everything
-- else keeps the JSON path so pre-dispatch outcomes retain their
-- spec-mandated HTTP statuses (404 for unknown methods, 202 for
-- notifications) — 'Wai.responseStream' would commit @200@ before dispatch
-- could veto it.
streamableMethods :: [Text]
streamableMethods =
  [ "tools/call", "tools/list"
  , "prompts/get", "prompts/list"
  , "resources/read", "resources/list", "resources/templates/list"
  , "completion/complete"
  ]

-- | Whether this request is answered with an SSE response stream.
wantsStreamingResponse :: BodyPeek -> Bool
wantsStreamingResponse peek =
  peekWantsStream peek
    && peekHasId peek
    && maybe False (`elem` streamableMethods) (peekMethod peek)

-- | Whether the body declares a modern (2026-07-28+) request.
peekIsModern :: BodyPeek -> Bool
peekIsModern peek =
  peekMetaVersion peek /= Nothing || peekMethod peek == Just "server/discover"

peekBody :: BSL.ByteString -> BodyPeek
peekBody body = case decode body of
  Just (Object o) -> BodyPeek
    { peekMethod = case KM.lookup "method" o of
        Just (String m) -> Just m
        _               -> Nothing
    , peekMetaVersion = metaProtocolVersion (KM.lookup "params" o)
    , peekName = case KM.lookup "params" o of
        Just (Object p) -> case (KM.lookup "name" p, KM.lookup "uri" p) of
          (Just (String n), _) -> Just n
          (_, Just (String u)) -> Just u
          _                    -> Nothing
        _ -> Nothing
    , peekId = case KM.lookup "id" o of
        Just v  -> either (const RequestIdNull) id (parseEither parseJSON v)
        Nothing -> RequestIdNull
    , peekHasId = KM.member "id" o
    , peekWantsStream = case KM.lookup "params" o of
        Just (Object p) -> case KM.lookup "_meta" p of
          Just (Object m) ->
            KM.member "progressToken" m
              || KM.member "io.modelcontextprotocol/logLevel" m
          _ -> False
        _ -> False
    }
  _ -> BodyPeek Nothing Nothing Nothing RequestIdNull False False

-- | Validate the request-metadata headers against the body. 'Nothing' means
-- the request may proceed; @'Just' err@ is answered with 400 and the given
-- JSON-RPC error.
--
-- Modern requests (body declares a protocol revision in @_meta@) follow the
-- 2026-07-28 rules: the MCP-Protocol-Version header must match the body's
-- declared revision, Mcp-Method must match the body method, and Mcp-Name
-- must match @params.name@/@params.uri@ for the three named methods
-- (@HeaderMismatch@, -32020); an undeclared revision yields
-- @UnsupportedProtocolVersionError@ (-32022).
--
-- Legacy requests keep the relaxed pre-2026 rules: only a
-- present-but-unsupported MCP-Protocol-Version header is rejected, and
-- @initialize@ is exempt entirely.
validateRequestHeaders :: Wai.Request -> BodyPeek -> Maybe JsonRpcError
validateRequestHeaders req peek = case peekMetaVersion peek of
  Just v
    | v `notElem` modernVersions -> Just (unsupportedVersionError v)
    | headerText "MCP-Protocol-Version" /= Just v ->
        Just $ headerMismatch $ "MCP-Protocol-Version header "
          <> maybe "is missing" (\h -> "value '" <> h <> "'") (headerText "MCP-Protocol-Version")
          <> " and does not match body value '" <> v <> "'"
    | headerText "Mcp-Method" /= peekMethod peek ->
        Just $ headerMismatch $ "Mcp-Method header "
          <> maybe "is missing" (\h -> "value '" <> h <> "'") (headerText "Mcp-Method")
          <> " and does not match body method"
    | needsName
    , (decodeSentinel <$> headerText "Mcp-Name") /= peekName peek ->
        Just $ headerMismatch $ "Mcp-Name header "
          <> maybe "is missing" (\h -> "value '" <> h <> "'") (headerText "Mcp-Name")
          <> " and does not match the body name/uri"
    | otherwise -> Nothing
  Nothing
    | peekMethod peek /= Just "initialize"
    , Just hv <- headerText "MCP-Protocol-Version"
    , hv `notElem` supportedVersions ++ modernVersions ->
        Just $ JsonRpcError
          { errorCode = -32600
          , errorMessage = "Unsupported protocol version. Supported versions: "
              <> T.intercalate ", " allVersions
          , errorData = Nothing
          }
    | otherwise -> Nothing
  where
    headerText name =
      TE.decodeUtf8With lenientDecode <$> lookup name (Wai.requestHeaders req)
    needsName = peekMethod peek `elem` map Just ["tools/call", "resources/read", "prompts/get"]
    headerMismatch msg = JsonRpcError
      { errorCode = -32020
      , errorMessage = "Header mismatch: " <> msg
      , errorData = Nothing
      }

-- | Decode the @=?base64?...?=@ sentinel encoding the 2026-07-28 transport
-- uses for header values that cannot be represented in plain ASCII. Values
-- not wrapped in the sentinel (or with invalid base64) pass through as-is.
decodeSentinel :: Text -> Text
decodeSentinel t
  | Just inner <- T.stripPrefix "=?base64?" t >>= T.stripSuffix "?=" =
      case B64.decode (TE.encodeUtf8 inner) of
        Right bs -> TE.decodeUtf8With lenientDecode bs
        Left _   -> t
  | otherwise = t

-- | Serve a subscriptions/listen request (2026-07-28): a long-lived SSE
-- stream that first acknowledges the subscription and then delivers exactly
-- the notification types the client opted into, tagged with the
-- subscription id. Closing the stream is the client's cancellation signal;
-- periodic SSE comments keep intermediaries from timing the stream out.
handleSubscriptionsListen :: HttpConfig -> NotificationSource -> BSL.ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleSubscriptionsListen config src body respond =
  case eitherDecode body :: Either String JsonRpcRequest of
    Left err -> do
      hPutStrLn stderr $ "subscriptions/listen parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ makeErrorResponse RequestIdNull $ JsonRpcError
          { errorCode = -32600
          , errorMessage = "Invalid Request"
          , errorData = Nothing
          })
    Right req -> do
      let subId = requestId req
          notifFilter = parseNotificationFilter (requestParams req)
      logVerbose config $ "Opening subscription stream " ++ show subId
      respond $ Wai.responseStream status200 sseHeaders $ \write flush -> do
          chan <- atomically $ subscribeEvents src
          let sendJson v = do
                write $ B.lazyByteString $ "data: " <> encode v <> "\n\n"
                flush
          sendJson $ toJSON $ acknowledgedNotification subId notifFilter
          forever $ do
            keepAlive <- registerDelay 25000000  -- 25s
            next <- atomically $
              (Just <$> readTChan chan)
                `orElse` (readTVar keepAlive >>= check >> pure Nothing)
            case next of
              Just event
                | filterAccepts notifFilter event ->
                    sendJson $ toJSON $ eventNotification subId event
                | otherwise -> pure ()
              Nothing -> do
                write $ B.byteString ": keep-alive\n\n"
                flush

-- | Serve a request that opted into request-scoped notifications: the
-- response is an SSE stream on which progress and client-log notifications
-- flow, followed by the final JSON-RPC response.
--
-- The handler runs in its own task, raced against a keep-alive writer that
-- doubles as the disconnect detector: when the client closes the stream the
-- next keep-alive write throws, the race cancels the handler task, and
-- nothing further is produced for the request.
handleStreamingRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers -> ClientContext -> BSL.ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleStreamingRequest config serverInfo handlers ctx body respond =
  respond $ Wai.responseStream status200 sseHeaders $ \write flush -> do
    -- One writer at a time, and a cancellation arriving mid-write cannot
    -- split an event in half
    streamLock <- newMVar ()
    let sendChunk b = withMVar streamLock $ \_ -> uninterruptibleMask_ $ do
          write b
          flush
        sendEvent v = sendChunk $ B.lazyByteString $ "data: " <> encode v <> "\n\n"
        emit n = sendEvent (toJSON (n :: JsonRpcNotification))
    case eitherDecode body of
      Left err -> do
        hPutStrLn stderr $ "JSON parse error: " ++ err
        sendEvent $ toJSON $ makeErrorResponse RequestIdNull $ JsonRpcError
          { errorCode = -32700, errorMessage = "Parse error", errorData = Nothing }
      Right jsonValue -> case parseJsonRpcMessage jsonValue of
        Left err -> do
          hPutStrLn stderr $ "JSON-RPC parse error: " ++ err
          sendEvent $ toJSON $ makeErrorResponse RequestIdNull $ JsonRpcError
            { errorCode = -32600, errorMessage = "Invalid Request", errorData = Nothing }
        Right message -> do
          logVerbose config $ "Processing streaming HTTP message: " ++ show (getMessageSummary message)
          -- 5s: short enough that an abandoned handler is cancelled
          -- promptly (a write to a closed connection is what surfaces the
          -- disconnect), long enough to stay negligible on the wire
          let keepAlive :: IO ()
              keepAlive = forever $ do
                threadDelay 5000000
                sendChunk $ B.byteString ": keep-alive\n\n"
          outcome <- race
            (handleMcpMessage serverInfo (httpCacheHints config)
              (httpNotifSupport config) emit handlers ctx message)
            keepAlive
          case outcome of
            Left (Just responseMsg) -> sendEvent (encodeJsonRpcMessage responseMsg)
            Left Nothing            -> pure ()
            Right ()                -> pure ()

-- | Handle JSON-RPC request from HTTP body
handleJsonRpcRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers -> ClientContext -> Bool -> BSL.ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleJsonRpcRequest config serverInfo handlers ctx modern body respond = do
  case eitherDecode body of
    Left err -> do
      hPutStrLn stderr $ "JSON parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ makeErrorResponse RequestIdNull $ JsonRpcError
          { errorCode = -32700
          , errorMessage = "Parse error"
          , errorData = Nothing
          })

    Right jsonValue -> handleSingleJsonRpc config serverInfo handlers ctx modern jsonValue respond

-- | Handle a single JSON-RPC message
handleSingleJsonRpc :: HttpConfig -> McpServerInfo -> McpServerHandlers -> ClientContext -> Bool -> Value -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleSingleJsonRpc config serverInfo handlers ctx modern jsonValue respond = do
  case parseJsonRpcMessage jsonValue of
    Left err -> do
      hPutStrLn stderr $ "JSON-RPC parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ makeErrorResponse RequestIdNull $ JsonRpcError
          { errorCode = -32600
          , errorMessage = "Invalid Request"
          , errorData = Nothing
          })

    Right message -> do
      logVerbose config $ "Processing HTTP message: " ++ show (getMessageSummary message)
      -- Single-JSON responses have no channel for request-scoped
      -- notifications; requests that want them take the streaming path.
      maybeResponse <- handleMcpMessage serverInfo (httpCacheHints config)
        (httpNotifSupport config) (\_ -> pure ()) handlers ctx message

      case maybeResponse of
        Just responseMsg -> do
          let responseJson = encode $ encodeJsonRpcMessage responseMsg
          logVerbose config $ "Sending HTTP response for: " ++ show (getMessageSummary message)
          respond $ Wai.responseLBS
            (statusForResponse modern responseMsg)
            [("Content-Type", "application/json")]
            responseJson

        Nothing -> do
          logVerbose config $ "No response needed for: " ++ show (getMessageSummary message)
          -- Accepted notification: 202 with no body, per the Streamable HTTP spec
          respond $ Wai.responseLBS status202 [] ""

-- | The HTTP status for a JSON-RPC response. Modern (2026-07-28) requests
-- map protocol-level failures onto HTTP statuses — 404 for an unknown RPC
-- method, 400 for an unsupported protocol version — so era-probing clients
-- can distinguish them; legacy requests always get 200 as before.
statusForResponse :: Bool -> JsonRpcMessage -> Status
statusForResponse True (JsonRpcMessageResponse r) = case responseError r of
  Just e | errorCode e == -32601 -> status404
         | errorCode e == -32022 -> status400
  _ -> status200
statusForResponse _ _ = status200
