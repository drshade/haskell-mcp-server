{-# LANGUAGE OverloadedStrings #-}

-- | Coverage for the change-notification machinery: the notifier, the
-- subscriptions/listen wire shapes, and era-aware capability advertisement.
module Spec.Subscriptions (spec) where

import Control.Concurrent.STM (atomically, readTChan)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import MCP.Server
import MCP.Server.Handlers (handleMcpMessage)
import MCP.Server.JsonRpc
import MCP.Server.Notifications
import Test.Hspec

subId :: RequestId
subId = RequestIdNumber 7

fullFilterParams :: Maybe Value
fullFilterParams = Just $ object
  [ "notifications" .= object
      [ "toolsListChanged" .= True
      , "resourceSubscriptions" .= (["resource://info"] :: [Text])
      ]
  ]

paramsOf :: JsonRpcNotification -> Object
paramsOf n = case notificationParams n of
  Just (Object o) -> o
  other           -> error $ "notification params not an object: " ++ show other

metaSubId :: Object -> Maybe Value
metaSubId o = do
  Object m <- KM.lookup "_meta" o
  KM.lookup "io.modelcontextprotocol/subscriptionId" m

spec :: Spec
spec = describe "Change notifications" $ do

  describe "Filter parsing" $ do
    it "parses the requested notification types" $ do
      let f = parseNotificationFilter fullFilterParams
      filterTools f `shouldBe` True
      filterPrompts f `shouldBe` False
      filterResources f `shouldBe` False
      filterResourceSubs f `shouldBe` ["resource://info"]

    it "treats missing filters as subscribed-to-nothing" $ do
      let f = parseNotificationFilter Nothing
      filterIsEmpty f `shouldBe` True

  describe "Filter semantics" $ do
    it "delivers only opted-in types" $ do
      let f = parseNotificationFilter fullFilterParams
      filterAccepts f ToolsListChangedEvent `shouldBe` True
      filterAccepts f PromptsListChangedEvent `shouldBe` False
      filterAccepts f ResourcesListChangedEvent `shouldBe` False

    it "matches resource updates by watched URI" $ do
      let f = parseNotificationFilter fullFilterParams
      filterAccepts f (ResourceUpdatedEvent "resource://info") `shouldBe` True
      filterAccepts f (ResourceUpdatedEvent "resource://other") `shouldBe` False

  describe "Wire shapes" $ do
    it "acknowledges with the subscription id and the honored subset" $ do
      let ack = acknowledgedNotification subId (parseNotificationFilter fullFilterParams)
      notificationMethod ack `shouldBe` "notifications/subscriptions/acknowledged"
      let o = paramsOf ack
      metaSubId o `shouldBe` Just (Number 7)
      KM.lookup "notifications" o `shouldBe` Just (object
        [ "toolsListChanged" .= True
        , "resourceSubscriptions" .= (["resource://info"] :: [Text])
        ])

    it "tags event notifications with the subscription id" $ do
      let n = eventNotification subId (ResourceUpdatedEvent "resource://info")
      notificationMethod n `shouldBe` "notifications/resources/updated"
      let o = paramsOf n
      metaSubId o `shouldBe` Just (Number 7)
      KM.lookup "uri" o `shouldBe` Just (String "resource://info")

    it "uses the standard list_changed methods" $ do
      notificationMethod (eventNotification subId ToolsListChangedEvent)
        `shouldBe` "notifications/tools/list_changed"
      notificationMethod (eventNotification subId PromptsListChangedEvent)
        `shouldBe` "notifications/prompts/list_changed"
      notificationMethod (eventNotification subId ResourcesListChangedEvent)
        `shouldBe` "notifications/resources/list_changed"

    it "legacy notifications are untagged" $ do
      let n = legacyEventNotification ToolsListChangedEvent
      notificationMethod n `shouldBe` "notifications/tools/list_changed"
      notificationParams n `shouldBe` Nothing

    it "closure responses carry resultType, the subscription id and the server identity" $ do
      let r = closureResponse (McpServerInfo "S" "2.0" "") subId
      responseResult r `shouldBe` Just (object
        [ "resultType" .= ("complete" :: Text)
        , "_meta" .= object
            [ "io.modelcontextprotocol/subscriptionId" .= (7 :: Int)
            , "io.modelcontextprotocol/serverInfo" .= object
                [ "name" .= ("S" :: Text), "version" .= ("2.0" :: Text) ]
            ]
        ])

  describe "Notifier plumbing" $ do
    it "delivers published events to subscribers" $ do
      (notifier, source) <- newMcpNotifier
      chan <- atomically $ subscribeEvents source
      notifyToolsListChanged notifier
      event <- atomically $ readTChan chan
      event `shouldBe` ToolsListChangedEvent

  describe "Era-aware capability advertisement" $ do
    let toolsServer = noHandlers
          { tools = Just (\_ -> pure [], \_ n _ -> pure (Left (UnknownTool n)))
          , resources = Just (\_ -> pure [], \_ _ -> pure (Left (ResourceNotFound "x")))
          }
        run support method params = handleMcpMessage
          (McpServerInfo "T" "1" "")
          defaultCacheHints
          support
          toolsServer
          anonymousContext
          (JsonRpcMessageRequest (JsonRpcRequest "2.0" (RequestIdNumber 1) method params))
        capsOf resp = case resp of
          Just (JsonRpcMessageResponse r)
            | Just (Object o) <- responseResult r
            , Just (Object caps) <- KM.lookup "capabilities" o -> pure caps
          other -> error $ "no capabilities in: " ++ show other
        initParams = Just $ object
          [ "protocolVersion" .= ("2025-11-25" :: Text)
          , "capabilities" .= object []
          , "clientInfo" .= object ["name" .= ("c" :: Text), "version" .= ("1" :: Text)]
          ]

    it "legacy initialize advertises listChanged only with legacy push" $ do
      caps <- capsOf =<< run (NotificationSupport True True) "initialize" initParams
      KM.lookup "tools" caps `shouldBe` Just (object ["listChanged" .= True])
      -- legacy subscribe (the removed resources/subscribe RPC) is never advertised
      KM.lookup "resources" caps `shouldBe` Just (object ["listChanged" .= True])

    it "legacy initialize over a push-less transport advertises neither" $ do
      caps <- capsOf =<< run (NotificationSupport False True) "initialize" initParams
      KM.lookup "tools" caps `shouldBe` Just (object [])
      KM.lookup "resources" caps `shouldBe` Just (object [])

    it "modern discover advertises listChanged and subscribe when listen is served" $ do
      caps <- capsOf =<< run (NotificationSupport False True) "server/discover" Nothing
      KM.lookup "tools" caps `shouldBe` Just (object ["listChanged" .= True])
      KM.lookup "resources" caps `shouldBe`
        Just (object ["subscribe" .= True, "listChanged" .= True])

    it "no support means nothing extra is advertised" $ do
      caps <- capsOf =<< run noNotificationSupport "server/discover" Nothing
      KM.lookup "tools" caps `shouldBe` Just (object [])