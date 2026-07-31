{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Stdio
  ( -- * STDIO Transport
    transportRunStdio
  ) where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
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

-- | Transport-specific implementation for STDIO
transportRunStdio :: (MonadIO m) => McpServerInfo -> McpServerHandlers m -> m ()
transportRunStdio serverInfo handlers = do
  -- Ensure UTF-8 encoding for all handles
  liftIO $ do
    hSetEncoding stderr utf8
    hSetEncoding stdout utf8
  loop
  where
    loop = do
      eof <- liftIO $ hIsEOF stdin
      if eof
        then liftIO $ TIO.hPutStrLn stderr "stdin closed - shutting down"
        else do
          input <- liftIO TIO.getLine
          unless (T.null $ T.strip input) $ do
            liftIO $ TIO.hPutStrLn stderr $ "Received request: " <> input
            case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 input) of
              Left err -> do
                liftIO $ TIO.hPutStrLn stderr $ "Parse error: " <> T.pack err
                sendResponse $ makeErrorResponse RequestIdNull $ JsonRpcError
                  { errorCode = -32700
                  , errorMessage = "Parse error"
                  , errorData = Nothing
                  }
              Right jsonValue ->
                case parseJsonRpcMessage jsonValue of
                  Left err -> do
                    liftIO $ TIO.hPutStrLn stderr $ "JSON-RPC parse error: " <> T.pack err
                    sendResponse $ makeErrorResponse RequestIdNull $ JsonRpcError
                      { errorCode = -32600
                      , errorMessage = "Invalid Request"
                      , errorData = Nothing
                      }
                  Right message -> do
                    liftIO $ TIO.hPutStrLn stderr $ "Processing message: " <> T.pack (show (getMessageSummary message))
                    response <- handleMcpMessage serverInfo handlers (ClientContext Nothing Nothing) message
                    case response of
                      Just responseMsg -> do
                        liftIO $ TIO.hPutStrLn stderr $ "Sending response for: " <> T.pack (show (getMessageSummary message))
                        sendMessage responseMsg
                      Nothing ->
                        liftIO $ TIO.hPutStrLn stderr $ "No response needed for: " <> T.pack (show (getMessageSummary message))
          loop

    sendMessage msg = sendRaw $ encode $ encodeJsonRpcMessage msg
    sendResponse resp = sendRaw $ encode $ toJSON resp
    sendRaw bytes = liftIO $ do
      TIO.putStrLn $ TE.decodeUtf8 $ BSL.toStrict bytes
      hFlush stdout
