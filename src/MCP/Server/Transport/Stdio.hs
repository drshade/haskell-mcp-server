{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Stdio
  ( -- * STDIO Transport
    transportRunStdio
  , transportRunStdioWithConfig
  , StdioConfig(..)
  , defaultStdioConfig
  ) where

import           Control.Monad          (unless, when)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           System.IO              (hFlush, hIsEOF, hSetEncoding, stderr,
                                         stdin, stdout, utf8)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
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
  } deriving (Show, Eq)

-- | Default STDIO configuration: summaries only, no raw bodies, no caching.
defaultStdioConfig :: StdioConfig
defaultStdioConfig = StdioConfig
  { stdioVerbose = False
  , stdioCacheHints = defaultCacheHints
  }

-- | Run the STDIO transport with the default configuration.
transportRunStdio :: McpServerInfo -> McpServerHandlers -> IO ()
transportRunStdio = transportRunStdioWithConfig defaultStdioConfig

-- | Run the STDIO transport with the given configuration.
transportRunStdioWithConfig :: StdioConfig -> McpServerInfo -> McpServerHandlers -> IO ()
transportRunStdioWithConfig config serverInfo handlers = do
  -- Ensure UTF-8 encoding for all handles
  hSetEncoding stderr utf8
  hSetEncoding stdout utf8
  loop
  where
    logLine = TIO.hPutStrLn stderr
    logVerbose msg = when (stdioVerbose config) $ logLine msg

    loop = do
      eof <- hIsEOF stdin
      if eof
        then logLine "stdin closed - shutting down"
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
                    response <- handleMcpMessage serverInfo (stdioCacheHints config) handlers anonymousContext message
                    case response of
                      Just responseMsg -> do
                        logLine $ "Sending response for: " <> T.pack (show (getMessageSummary message))
                        sendMessage responseMsg
                      Nothing ->
                        logLine $ "No response needed for: " <> T.pack (show (getMessageSummary message))
          loop

    sendMessage msg = sendRaw $ encode $ encodeJsonRpcMessage msg
    sendResponse resp = sendRaw $ encode $ toJSON resp
    sendRaw bytes = do
      TIO.putStrLn $ TE.decodeUtf8 $ BSL.toStrict bytes
      hFlush stdout
