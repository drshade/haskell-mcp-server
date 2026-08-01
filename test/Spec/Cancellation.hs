{-# LANGUAGE OverloadedStrings #-}

-- | Coverage for the cancellation contract (ADR 0008): a request task
-- cancelled mid-handler stops emitting, never produces a response, and
-- releases bracket-acquired resources. The transports build on exactly
-- this shape (an 'async' around 'handleMcpMessage' that 'cancel'
-- interrupts), so the properties verified here are the ones the wire
-- behavior depends on.
module Spec.Cancellation (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket_)
import Data.Aeson
import Data.IORef
import Data.Text (Text)
import MCP.Server
import MCP.Server.Handlers (handleMcpMessage)
import MCP.Server.JsonRpc
import Test.Hspec

-- A slow tools/call carrying a progressToken (so emissions are live),
-- driven from an async that the test cancels once the handler signals
-- it has started.
runCancelled :: (ClientContext -> IO ()) -> IO [JsonRpcNotification]
runCancelled handlerBody = do
  sink <- newIORef []
  started <- newEmptyMVar
  responded <- newIORef False
  let handlers = noHandlers
        { tools = Just
            ( \_ -> pure []
            , \ctx _ _ -> do
                reportProgress ctx 0 (Just 1) Nothing
                putMVar started ()
                handlerBody ctx
                pure $ Right $ toToolResult ("done" :: Text)
            )
        }
      params = object
        [ "name" .= ("slow" :: Text)
        , "arguments" .= object []
        , "_meta" .= object ["progressToken" .= ("t" :: Text)]
        ]
  task <- async $ do
    resp <- handleMcpMessage (McpServerInfo "T" "1" "") defaultCacheHints
      noNotificationSupport (\n -> modifyIORef' sink (++ [n]))
      handlers anonymousContext
      (JsonRpcMessageRequest (JsonRpcRequest "2.0" (RequestIdNumber 1) "tools/call" (Just params)))
    case resp of
      Just _  -> writeIORef responded True
      Nothing -> pure ()
  takeMVar started
  cancel task  -- waits for the task to finish
  _ <- waitCatch task
  readIORef responded `shouldReturn` False
  readIORef sink

spec :: Spec
spec = describe "Cancellation contract" $ do

  it "a cancelled handler stops emitting and never yields a response" $ do
    lateEmit <- newIORef False
    ns <- runCancelled $ \ctx -> do
      threadDelay 5000000
      writeIORef lateEmit True
      reportProgress ctx 1 (Just 1) Nothing
    -- only the pre-cancellation progress made it out
    map notificationMethod ns `shouldBe` ["notifications/progress"]
    readIORef lateEmit `shouldReturn` False

  it "bracket releases handler resources on cancellation" $ do
    acquired <- newIORef False
    released <- newIORef False
    _ <- runCancelled $ \_ ->
      bracket_ (writeIORef acquired True) (writeIORef released True) $
        threadDelay 5000000
    readIORef acquired `shouldReturn` True
    readIORef released `shouldReturn` True
