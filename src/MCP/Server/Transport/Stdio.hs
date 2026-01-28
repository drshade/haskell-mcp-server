{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Stdio
  ( -- * STDIO Transport
    transportRunStdio
  ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           System.IO              (hFlush, stderr, stdout)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
import           MCP.Server.Types


-- | Transport-specific implementation for STDIO
import           System.IO              (hSetEncoding, utf8)

transportRunStdio :: (MonadIO m) => McpServerInfo -> McpServerHandlers m -> m ()
transportRunStdio serverInfo handlers = do
  -- Ensure UTF-8 encoding for all handles
  liftIO $ do
    hSetEncoding stderr utf8
    hSetEncoding stdout utf8
  loop
  where
    loop = do
      input <- liftIO TIO.getLine
      when (not $ T.null $ T.strip input) $ do
        -- Use TIO.hPutStrLn for UTF-8 output
        liftIO $ TIO.hPutStrLn stderr $ "Received request: " <> input
        case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 input) of
          Left err -> liftIO $ TIO.hPutStrLn stderr $ "Parse error: " <> T.pack err
          Right jsonValue -> do
            case parseJsonRpcMessage jsonValue of
              Left err -> liftIO $ TIO.hPutStrLn stderr $ "JSON-RPC parse error: " <> T.pack err
              Right message -> do
                liftIO $ TIO.hPutStrLn stderr $ "Processing message: " <> T.pack (show (getMessageSummary message))
                response <- handleMcpMessage serverInfo handlers message
                case response of
                  Just responseMsg -> do
                    liftIO $ TIO.hPutStrLn stderr $ "Sending response for: " <> T.pack (show (getMessageSummary message))
                    let responseText = TE.decodeUtf8 $ BSL.toStrict $ encode $ encodeJsonRpcMessage responseMsg
                    liftIO $ TIO.putStrLn responseText
                    liftIO $ hFlush stdout
                  Nothing -> liftIO $ TIO.hPutStrLn stderr $ "No response needed for: " <> T.pack (show (getMessageSummary message))
        loop
