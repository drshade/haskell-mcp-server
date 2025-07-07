{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Stdio
  ( -- * STDIO Transport
    transportRunStdio
  ) where

import           Control.Exception      (catch, IOException)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           System.IO              (hFlush, stderr, stdout)
import           System.IO.Error        (isEOFError)

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
      -- Catch EOF exception when stdin is closed
      inputResult <- liftIO $ (Just <$> TIO.getLine) `catch` handleEOF
      case inputResult of
        Nothing ->
          -- EOF encountered, exit gracefully
          liftIO $ TIO.hPutStrLn stderr "MCP server: stdin closed (EOF), shutting down gracefully"
        Just input
          | not $ T.null $ T.strip input -> do
              -- Use TIO.hPutStrLn for UTF-8 output
              liftIO $ TIO.hPutStrLn stderr $ "Received request: " <> input
              case eitherDecode $ BSL.fromStrict $ TE.encodeUtf8 input of
                Left err -> do
                  liftIO $ TIO.hPutStrLn stderr $ "Parse error: " <> T.pack err
                  loop
                Right jsonValue -> do
                  case parseJsonRpcMessage jsonValue of
                    Left err -> do
                      liftIO $ TIO.hPutStrLn stderr $ "JSON-RPC parse error: " <> T.pack err
                      loop
                    Right message -> do
                      liftIO $ TIO.hPutStrLn stderr $ "Processing message: " <> T.pack (show (getMessageSummary message))
                      response <- handleMcpMessage serverInfo handlers message
                      case response of
                        Just responseMsg -> do
                          liftIO $ TIO.hPutStrLn stderr $ "Sending response for: " <> T.pack (show (getMessageSummary message))
                          let responseText = TE.decodeUtf8 $ BSL.toStrict $ encode $ encodeJsonRpcMessage responseMsg
                          -- Handle broken pipe when writing response
                          writeResult <- liftIO $ (TIO.putStrLn responseText >> hFlush stdout >> return True) `catch` handleBrokenPipe
                          if writeResult
                            then loop  -- Continue normally
                            else liftIO $ TIO.hPutStrLn stderr "MCP server: stdout closed (broken pipe), shutting down" -- Signal to stop looping
                        Nothing -> do
                          liftIO $ TIO.hPutStrLn stderr $ "No response needed for: " <> T.pack (show (getMessageSummary message))
                          loop
          | otherwise -> loop

    handleEOF :: IOException -> IO (Maybe T.Text)
    handleEOF e
      | isEOFError e = return Nothing
      | otherwise = ioError e  -- Re-throw non-EOF exceptions

    handleBrokenPipe :: IOException -> IO Bool
    handleBrokenPipe e
      | isEOFError e = return False  -- Treat EOF on stdout as broken pipe
      | "Broken pipe" `T.isInfixOf` T.pack (show e) = return False
      | "Resource vanished" `T.isInfixOf` T.pack (show e) = return False
      | otherwise = ioError e  -- Re-throw other exceptions

