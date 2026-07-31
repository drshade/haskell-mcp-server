{-# LANGUAGE OverloadedStrings #-}

-- | Server-initiated change notifications.
--
-- Create a notifier with 'newMcpNotifier', hand the 'NotificationSource' to
-- a transport via its configuration ('MCP.Server.Transport.Stdio.StdioConfig'
-- \/ 'MCP.Server.Transport.Http.HttpConfig'), keep the 'McpNotifier', and
-- call its actions whenever your tool\/prompt\/resource lists (or a specific
-- resource) change:
--
-- > (notifier, source) <- newMcpNotifier
-- > let config = defaultStdioConfig { stdioNotifications = Just source }
-- > _ <- forkIO $ appLogic notifier
-- > runMcpServerStdioWithConfig config serverInfo handlers
--
-- Delivery is transport- and era-specific: modern (2026-07-28) clients open
-- a @subscriptions\/listen@ stream and receive only the notification types
-- they opted into, tagged with their subscription id; legacy stdio clients
-- receive untagged notifications spontaneously after @initialize@. Legacy
-- HTTP has no delivery channel (this library dropped the deprecated GET SSE
-- stream), so the @listChanged@ capabilities are not advertised there.
module MCP.Server.Notifications
  ( -- * Notifier
    McpNotifier(..)
  , NotificationSource
  , newMcpNotifier
  , ChangeEvent(..)
  , subscribeEvents

    -- * Subscription wire format (used by transports)
  , NotificationFilter(..)
  , parseNotificationFilter
  , filterAccepts
  , filterIsEmpty
  , acknowledgedNotification
  , eventNotification
  , legacyEventNotification
  , closureResponse
  ) where

import           Control.Concurrent.STM (STM, TChan, atomically,
                                         dupTChan, newBroadcastTChanIO,
                                         writeTChan)
import           Data.Aeson
import qualified Data.Aeson.KeyMap      as KM
import           Data.Aeson.Types       (Pair)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.URI            (URI)

import           MCP.Server.JsonRpc

-- | A change an application can announce.
data ChangeEvent
  = ToolsListChangedEvent
  | PromptsListChangedEvent
  | ResourcesListChangedEvent
  | ResourceUpdatedEvent Text  -- ^ the resource URI, rendered
  deriving (Show, Eq)

-- | The application-facing handle: call these when things change.
data McpNotifier = McpNotifier
  { notifyToolsListChanged     :: IO ()
  , notifyPromptsListChanged   :: IO ()
  , notifyResourcesListChanged :: IO ()
  , notifyResourceUpdated      :: URI -> IO ()
  }

-- | The transport-facing end of a notifier: subscribe with
-- 'subscribeEvents'. Backed by a broadcast 'TChan', so events published
-- while nobody listens are dropped rather than retained.
newtype NotificationSource = NotificationSource (TChan ChangeEvent)

-- | Create a notifier and its source.
newMcpNotifier :: IO (McpNotifier, NotificationSource)
newMcpNotifier = do
  chan <- newBroadcastTChanIO
  let publish = atomically . writeTChan chan
  pure
    ( McpNotifier
        { notifyToolsListChanged     = publish ToolsListChangedEvent
        , notifyPromptsListChanged   = publish PromptsListChangedEvent
        , notifyResourcesListChanged = publish ResourcesListChangedEvent
        , notifyResourceUpdated      = publish . ResourceUpdatedEvent . T.pack . show
        }
    , NotificationSource chan
    )

-- | A private read end delivering every event published after the call.
subscribeEvents :: NotificationSource -> STM (TChan ChangeEvent)
subscribeEvents (NotificationSource chan) = dupTChan chan

-- | Which notification types a @subscriptions\/listen@ request opted into.
data NotificationFilter = NotificationFilter
  { filterTools        :: Bool
  , filterPrompts      :: Bool
  , filterResources    :: Bool
  , filterResourceSubs :: [Text]  -- ^ URIs watched for updates
  } deriving (Show, Eq)

-- | Parse the @notifications@ filter out of a @subscriptions\/listen@
-- request's params. Absent fields mean "not subscribed".
parseNotificationFilter :: Maybe Value -> NotificationFilter
parseNotificationFilter params = NotificationFilter
  { filterTools        = boolField "toolsListChanged"
  , filterPrompts      = boolField "promptsListChanged"
  , filterResources    = boolField "resourcesListChanged"
  , filterResourceSubs = subsField
  }
  where
    notifications = do
      Object o <- params
      KM.lookup "notifications" o
    boolField key = case notifications of
      Just (Object n) | Just (Bool b) <- KM.lookup key n -> b
      _ -> False
    subsField = case notifications of
      Just (Object n) | Just (Array xs) <- KM.lookup "resourceSubscriptions" n ->
        [u | String u <- foldr (:) [] xs]
      _ -> []

-- | Whether a subscription with this filter receives the event.
filterAccepts :: NotificationFilter -> ChangeEvent -> Bool
filterAccepts f ToolsListChangedEvent      = filterTools f
filterAccepts f PromptsListChangedEvent    = filterPrompts f
filterAccepts f ResourcesListChangedEvent  = filterResources f
filterAccepts f (ResourceUpdatedEvent uri) = uri `elem` filterResourceSubs f

-- | True when the filter subscribes to nothing at all.
filterIsEmpty :: NotificationFilter -> Bool
filterIsEmpty f =
  not (filterTools f || filterPrompts f || filterResources f)
    && null (filterResourceSubs f)

-- | The @_meta@ object tagging a message with its subscription id (the
-- JSON-RPC id of the @subscriptions\/listen@ request).
subscriptionMeta :: RequestId -> Pair
subscriptionMeta subId =
  "_meta" .= object ["io.modelcontextprotocol/subscriptionId" .= subId]

-- | The mandatory first message of a subscription stream: the honored
-- filter (this library honors every requested type).
acknowledgedNotification :: RequestId -> NotificationFilter -> JsonRpcNotification
acknowledgedNotification subId f =
  makeNotification "notifications/subscriptions/acknowledged" $ Just $ object
    [ subscriptionMeta subId
    , "notifications" .= object (concat
        [ ["toolsListChanged" .= True | filterTools f]
        , ["promptsListChanged" .= True | filterPrompts f]
        , ["resourcesListChanged" .= True | filterResources f]
        , ["resourceSubscriptions" .= filterResourceSubs f | not (null (filterResourceSubs f))]
        ])
    ]

-- | The method and extra params of an event's notification.
eventBody :: ChangeEvent -> (Text, [Pair])
eventBody ToolsListChangedEvent      = ("notifications/tools/list_changed", [])
eventBody PromptsListChangedEvent    = ("notifications/prompts/list_changed", [])
eventBody ResourcesListChangedEvent  = ("notifications/resources/list_changed", [])
eventBody (ResourceUpdatedEvent uri) = ("notifications/resources/updated", ["uri" .= uri])

-- | A change event as a subscription-tagged notification (modern era).
eventNotification :: RequestId -> ChangeEvent -> JsonRpcNotification
eventNotification subId event =
  let (method, extra) = eventBody event
  in makeNotification method $ Just $ object (subscriptionMeta subId : extra)

-- | A change event as an untagged notification (legacy stdio delivery).
legacyEventNotification :: ChangeEvent -> JsonRpcNotification
legacyEventNotification event =
  let (method, extra) = eventBody event
  in makeNotification method $ if null extra then Nothing else Just (object extra)

-- | The graceful-closure response a server sends when it ends a
-- subscription on its own initiative (e.g. shutdown).
closureResponse :: RequestId -> JsonRpcResponse
closureResponse subId = makeSuccessResponse subId $ object
  [ "resultType" .= ("complete" :: Text)
  , subscriptionMeta subId
  ]
