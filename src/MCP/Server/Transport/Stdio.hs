{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Stdio
  ( -- * STDIO Transport
    transportRunStdio
  , transportRunStdioWithConfig
  , StdioConfig(..)
  , defaultStdioConfig
  ) where

import           Control.Concurrent     (ThreadId, forkIO, killThread)
import           Control.Concurrent.Async (Async, async, cancel)
import           Control.Concurrent.MVar (modifyMVar_, newEmptyMVar, newMVar,
                                          putMVar, readMVar, takeMVar,
                                          withMVar)
import           Control.Concurrent.STM (atomically, readTChan)
import           Control.Exception      (finally, uninterruptibleMask_)
import           Control.Monad          (forever, unless, when)
import           Data.Aeson
import qualified Data.Aeson.KeyMap      as KM
import           Data.Aeson.Types       (parseEither)
import qualified Data.ByteString.Lazy   as BSL
import           Data.IORef             (newIORef, readIORef, writeIORef)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           System.IO              (hFlush, hIsEOF, hSetEncoding, stderr,
                                         stdin, stdout, utf8)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
import           MCP.Server.Notifications
import           MCP.Server.Protocol    (modernVersions)
import           MCP.Server.Types

-- | STDIO transport configuration
data StdioConfig = StdioConfig
  { stdioVerbose :: Bool
    -- ^ When 'True', raw request bodies are logged to stderr. The default
    -- ('False') logs only message summaries (method and id): tool arguments
    -- may carry sensitive data that does not belong in logs.
  , stdioCacheHints :: CacheHints
    -- ^ Cacheability hints stamped onto modern (2026-07-28+) list/read
    -- results.
  , stdioNotifications :: Maybe NotificationSource
    -- ^ When configured, change notifications are delivered: modern clients
    -- via @subscriptions\/listen@, legacy clients as spontaneous
    -- notifications after @initialize@ — and the corresponding
    -- @listChanged@\/@subscribe@ capabilities are advertised.
  }

-- | Default STDIO configuration: summaries only, no raw bodies, no caching,
-- no notifications.
defaultStdioConfig :: StdioConfig
defaultStdioConfig = StdioConfig
  { stdioVerbose = False
  , stdioCacheHints = defaultCacheHints
  , stdioNotifications = Nothing
  }

-- | Run the STDIO transport with the default configuration.
transportRunStdio :: McpServerInfo -> McpServerHandlers -> IO ()
transportRunStdio = transportRunStdioWithConfig defaultStdioConfig

-- | Run the STDIO transport with the given configuration.
--
-- Each request is served in its own task, so a @notifications/cancelled@
-- naming its id can interrupt it mid-flight (after which nothing further is
-- written for that id). Cancellation reaches handler code as an
-- asynchronous exception: handlers that acquire resources should release
-- them with 'Control.Exception.bracket'.
--
-- This also means requests run /concurrently/ (before 0.2.0.1 the stdio
-- transport processed them strictly sequentially): handlers touching
-- shared mutable state must synchronize, as was already required of
-- handlers used with the HTTP transport.
transportRunStdioWithConfig :: StdioConfig -> McpServerInfo -> McpServerHandlers -> IO ()
transportRunStdioWithConfig config serverInfo handlers = do
  -- Ensure UTF-8 encoding for all handles
  hSetEncoding stderr utf8
  hSetEncoding stdout utf8

  -- Subscription threads, request tasks and the main loop share stdout:
  -- one line at a time.
  writeLock <- newMVar ()
  -- Active subscriptions_listen streams, by their request id
  subsVar <- newMVar ([] :: [(RequestId, ThreadId)])
  -- In-flight request tasks, by request id (cancellable)
  inflightVar <- newMVar ([] :: [(RequestId, Async ())])
  -- Whether a legacy client has completed initialize (gates legacy pushes)
  legacyReady <- newIORef False

  let logLine = TIO.hPutStrLn stderr
      logVerbose msg = when (stdioVerbose config) $ logLine msg

      -- Writes are uninterruptible so a cancellation arriving mid-write
      -- cannot corrupt the message channel with a half line
      sendRaw bytes = withMVar writeLock $ \_ -> uninterruptibleMask_ $ do
        TIO.putStrLn $ TE.decodeUtf8 $ BSL.toStrict bytes
        hFlush stdout
      sendMessage msg = sendRaw $ encode $ encodeJsonRpcMessage msg
      sendResponse resp = sendRaw $ encode $ toJSON (resp :: JsonRpcResponse)
      sendNotification notif = sendRaw $ encode $ toJSON (notif :: JsonRpcNotification)

      notifSupport = case stdioNotifications config of
        Nothing -> noNotificationSupport
        Just _  -> NotificationSupport { supportsLegacyPush = True, supportsListen = True }

  -- Legacy delivery: push untagged notifications to a client that completed
  -- the initialize handshake (which is when the capability was advertised).
  case stdioNotifications config of
    Nothing  -> pure ()
    Just src -> do
      chan <- atomically $ subscribeEvents src
      _ <- forkIO $ forever $ do
        event <- atomically $ readTChan chan
        ready <- readIORef legacyReady
        when ready $ sendNotification $ legacyEventNotification event
      pure ()

  let
    -- Open a subscriptions/listen stream: acknowledge, then deliver
    -- matching events tagged with the subscription id until cancelled.
    -- A listen reusing an already-open subscription id replaces the old
    -- stream (otherwise the shadowed writer would leak until EOF).
    openSubscription src req = do
      let subId = requestId req
          notifFilter = parseNotificationFilter (requestParams req)
      _ <- cancelSubscription subId
      chan <- atomically $ subscribeEvents src
      sendNotification $ acknowledgedNotification subId notifFilter
      tid <- forkIO $ forever $ do
        event <- atomically $ readTChan chan
        when (filterAccepts notifFilter event) $
          sendNotification $ eventNotification subId event
      modifyMVar_ subsVar (pure . ((subId, tid) :))
      logLine $ "Opened subscription " <> T.pack (show subId)

    cancelSubscription cancelledId = do
      subs <- readMVar subsVar
      case lookup cancelledId subs of
        Nothing  -> pure False
        Just tid -> do
          killThread tid
          modifyMVar_ subsVar (pure . filter ((/= cancelledId) . fst))
          logLine $ "Cancelled subscription " <> T.pack (show cancelledId)
          pure True

    -- Server-side teardown at EOF: graceful closure for every open stream
    closeAllSubscriptions = do
      subs <- readMVar subsVar
      mapM_ (\(subId, tid) -> do
                killThread tid
                sendResponse $ closureResponse serverInfo subId)
            subs

    -- Cancel an in-flight request task. 'cancel' waits for the task to
    -- finish, so once this returns nothing further is written for that id.
    cancelInflight cancelledId = do
      inflight <- readMVar inflightVar
      case lookup cancelledId inflight of
        Nothing -> pure False
        Just task -> do
          cancel task
          logLine $ "Cancelled request " <> T.pack (show cancelledId)
          pure True

    dispatchMessage message = do
      -- Request-scoped notifications (progress, client logs) share the
      -- locked stdout channel, interleaved before the response
      response <- handleMcpMessage serverInfo (stdioCacheHints config) notifSupport sendNotification handlers anonymousContext message
      case response of
        Just responseMsg -> do
          logLine $ "Sending response for: " <> T.pack (show (getMessageSummary message))
          sendMessage responseMsg
        Nothing ->
          logLine $ "No response needed for: " <> T.pack (show (getMessageSummary message))

    handleParsed message = case message of
      -- subscriptions/listen is transport-level: the stream outlives the
      -- request. Only intercept when a source is configured and the
      -- declared revision (if any) is one we implement — otherwise fall
      -- through for the ordinary -32601 / -32022 answer.
      JsonRpcMessageRequest req
        | requestMethod req == "subscriptions/listen"
        , Just src <- stdioNotifications config
        , maybe True (`elem` modernVersions) (metaProtocolVersion (requestParams req))
        -> openSubscription src req

      -- Every other request runs in its own task so a later
      -- notifications/cancelled can interrupt it while the read loop keeps
      -- serving. The task is registered before its body starts (the gate)
      -- so cancellation can never race registration; it deregisters itself
      -- on any exit, including cancellation.
        | otherwise -> do
          let rid = requestId req
          gate <- newEmptyMVar
          task <- async $ do
            takeMVar gate
            dispatchMessage message
              `finally` modifyMVar_ inflightVar (pure . filter ((/= rid) . fst))
          modifyMVar_ inflightVar (pure . ((rid, task) :))
          putMVar gate ()

      -- notifications/cancelled tears down the referenced subscription or
      -- in-flight request (no response either way, per the cancellation
      -- rules; unknown ids are ignored as the spec requires)
      JsonRpcMessageNotification notif
        | notificationMethod notif == "notifications/cancelled"
        , Just cancelledId <- cancelledRequestId (notificationParams notif)
        -> do
          wasSub <- cancelSubscription cancelledId
          unless wasSub $ do
            wasInflight <- cancelInflight cancelledId
            unless wasInflight $
              logLine $ "Ignoring cancellation for unknown request " <> T.pack (show cancelledId)

      _ -> do
        -- The client's initialized notification is the legacy ready signal:
        -- the lifecycle forbids server notifications before it arrives, and
        -- it only arrives after the client accepted a successful handshake
        -- response (so failed initializes never enable pushes).
        case message of
          JsonRpcMessageNotification n
            | notificationMethod n == "notifications/initialized" ->
                writeIORef legacyReady True
          _ -> pure ()
        dispatchMessage message

    loop = do
      eof <- hIsEOF stdin
      if eof
        then do
          inflight <- readMVar inflightVar
          mapM_ (cancel . snd) inflight
          closeAllSubscriptions
          logLine "stdin closed - shutting down"
        else do
          input <- TIO.getLine
          unless (T.null $ T.strip input) $ do
            logVerbose $ "Received request: " <> input
            case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 input) of
              Left err -> do
                logLine $ "Parse error: " <> T.pack err
                sendResponse $ makeErrorResponse RequestIdNull $ JsonRpcError
                  { errorCode = -32700
                  , errorMessage = "Parse error"
                  , errorData = Nothing
                  }
              Right jsonValue ->
                case parseJsonRpcMessage jsonValue of
                  Left err -> do
                    logLine $ "JSON-RPC parse error: " <> T.pack err
                    sendResponse $ makeErrorResponse RequestIdNull $ JsonRpcError
                      { errorCode = -32600
                      , errorMessage = "Invalid Request"
                      , errorData = Nothing
                      }
                  Right message -> do
                    logLine $ "Processing message: " <> T.pack (show (getMessageSummary message))
                    handleParsed message
          loop

  loop

-- | The request id referenced by a notifications/cancelled notification.
cancelledRequestId :: Maybe Value -> Maybe RequestId
cancelledRequestId params = do
  Object o <- params
  v <- KM.lookup "requestId" o
  either (const Nothing) Just (parseEither parseJSON v)
