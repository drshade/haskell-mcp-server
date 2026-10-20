{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Coverage for the derived interface of multi round-trip requests (ADR
-- 0009, phase 3): derived forms, and derived handlers that return 'Ask'.
module Spec.DerivedAsk (spec) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap   as KM
import           Data.Aeson.Types    (Pair)
import qualified Data.Map            as Map
import           Data.Proxy          (Proxy (..))
import           Data.Text           (Text)
import           MCP.Server
import           MCP.Server.Derive
import           MCP.Server.Handlers (handleMcpMessage)
import           MCP.Server.JsonRpc
import           Test.Hspec
import           TestHelpers         (testStateCodec)

import           AskTypes

handlers :: McpServerHandlers
handlers = noHandlers
  { tools = Just $(deriveToolHandler ''OpsTool 'handleOps)
  , prompts = Just $(derivePromptHandler ''OpsPrompt 'handlePrompt)
  , resources = Just $(deriveResourceHandler ''OpsResource 'handleResource)
  }

ioTools :: (ToolListHandler, ToolCallHandler)
ioTools = $(deriveToolHandler ''OpsTool 'handleOpsIO)

request :: McpServerHandlers -> Text -> Value -> [Pair] -> IO Value
request hs method caps params = do
  response <- handleMcpMessage (McpServerInfo "T" "1" "") defaultCacheHints testStateCodec
    noNotificationSupport (\_ -> pure ()) hs anonymousContext $
      JsonRpcMessageRequest JsonRpcRequest
        { requestJsonrpc = "2.0"
        , requestId = RequestIdNumber 1
        , requestMethod = method
        , requestParams = Just $ object $ params ++
            [ "_meta" .= object
                [ "io.modelcontextprotocol/protocolVersion" .= ("2026-07-28" :: Text)
                , "io.modelcontextprotocol/clientCapabilities" .= caps
                ]
            ]
        }
  case response of
    Just (JsonRpcMessageResponse r) -> pure (toJSON r)
    other -> fail ("expected a response, got " ++ show other)

formCaps, noCaps :: Value
formCaps = object ["elicitation" .= object ["form" .= object []]]
noCaps = object []

at :: [Key] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup k o >>= at ks
at _ _ = Nothing

-- | The single request of an @input_required@ result: its key and params.
questionOf :: Value -> Maybe (Key, Value)
questionOf v = case at ["result", "inputRequests"] v of
  Just (Object rs) | [(k, r)] <- KM.toList rs -> (,) k <$> at ["params"] r
  _ -> Nothing

-- | The params of a retry answering the given result's question.
answering :: Value -> [Pair] -> [Pair]
answering previous content = case (questionOf previous, at ["result", "requestState"] previous) of
  (Just (key, _), Just st) ->
    [ "inputResponses" .= object [key .= object ["action" .= ("accept" :: Text), "content" .= object content]]
    , "requestState" .= st
    ]
  _ -> error ("not an input_required result: " ++ show previous)

firstText :: Value -> Maybe Value
firstText v = case at ["result", "content"] v of
  Just (Array xs) | (c : _) <- foldr (:) [] xs -> at ["text"] c
  _ -> case at ["result", "messages"] v of
    Just (Array xs) | (m : _) <- foldr (:) [] xs -> at ["content", "text"] m
    _ -> case at ["result", "contents"] v of
      Just (Array xs) | (c : _) <- foldr (:) [] xs -> at ["text"] c
      _ -> Nothing

callDeploy :: [Pair] -> [Pair]
callDeploy extra = ["name" .= ("deploy" :: Text), "arguments" .= object ["service" .= ("billing" :: Text)]] ++ extra

spec :: Spec
spec = describe "Derived handlers that ask" $ do
  describe "deriveForm" $ do
    it "derives a flat schema from a record" $
      toJSON (formSchema (Proxy :: Proxy DeployTarget)) `shouldBe` object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "environment" .= object ["type" .= ("string" :: Text), "enum" .= (["staging", "production"] :: [Text])]
            , "reason" .= object ["type" .= ("string" :: Text), "description" .= ("Why deploy now?" :: Text)]
            , "notify" .= object ["type" .= ("boolean" :: Text)]
            , "replicas" .= object ["type" .= ("integer" :: Text)]
            , "tags" .= object
                [ "type" .= ("array" :: Text)
                , "items" .= object ["type" .= ("string" :: Text), "enum" .= (["urgent", "routine"] :: [Text])]
                ]
            , "ticket" .= object ["type" .= ("string" :: Text)]
            ]
        , "required" .= (["environment", "reason", "replicas", "tags"] :: [Text])
        ]

    it "adds titles and defaults from the options" $
      toJSON (formSchema (Proxy :: Proxy Styled)) `shouldBe` object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "size" .= object ["type" .= ("number" :: Text), "title" .= ("Size" :: Text), "default" .= (1.5 :: Double)]
            , "label" .= object ["type" .= ("string" :: Text)]
            , "mode" .= object
                [ "type" .= ("string" :: Text), "enum" .= (["staging", "production"] :: [Text])
                , "default" .= ("production" :: Text), "description" .= ("Where" :: Text)
                ]
            ]
        , "required" .= (["size", "label", "mode"] :: [Text])
        ]

    it "parses submitted content" $
      formParse (Map.fromList
        [ ("environment", "production"), ("reason", "hotfix"), ("replicas", Number 3)
        , ("tags", toJSON ["urgent" :: Text]), ("ticket", "OPS-1") ])
        `shouldBe` Right (DeployTarget Production "hotfix" Nothing 3 [Urgent] (Just (Ticket "OPS-1")))

    it "names the field when content does not fit" $
      (formParse (Map.fromList [("proceed", "maybe")]) :: Either Text Confirm)
        `shouldBe` Left "field 'proceed': Failed to parse Bool from: maybe"

    it "rejects types a form cannot hold at compile time" $ do
      rejectedNested `shouldBe` True
      rejectedTextList `shouldBe` True
      rejectedUnit `shouldBe` True
      rejectedUnknownField `shouldBe` True
      rejectedBadDefault `shouldBe` True
      rejectedBadChoice `shouldBe` True
      rejectedFractionalInt `shouldBe` True
      rejectedOutOfRangeInt `shouldBe` True

    it "keeps whole-number and non-ASCII defaults intact" $
      toJSON (formSchema (Proxy :: Proxy Counted)) `shouldBe` object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "count" .= object ["type" .= ("integer" :: Text), "default" .= (3 :: Int)]
            , "greeting" .= object ["type" .= ("string" :: Text), "default" .= ("café 東京" :: Text)]
            ]
        , "required" .= (["count", "greeting"] :: [Text])
        ]

  describe "derive*Handler with Ask" $ do
    it "runs a derived tool across rounds" $ do
      r1 <- request handlers "tools/call" formCaps (callDeploy [])
      fmap fst (questionOf r1) `shouldBe` Just "0"
      fmap (at ["message"] . snd) (questionOf r1) `shouldBe` Just (Just "Where should billing deploy to?")
      fmap (at ["requestedSchema", "required"] . snd) (questionOf r1)
        `shouldBe` Just (Just (toJSON (["environment", "reason", "replicas", "tags"] :: [Text])))
      r2 <- request handlers "tools/call" formCaps $ callDeploy $ answering r1
        [ "environment" .= ("staging" :: Text), "reason" .= ("fix" :: Text)
        , "replicas" .= (2 :: Int), "tags" .= ([] :: [Text]) ]
      fmap (at ["message"] . snd) (questionOf r2) `shouldBe` Just (Just "Apply Staging x2?")
      r3 <- request handlers "tools/call" formCaps $ callDeploy $ answering r2 ["proceed" .= True]
      firstText r3 `shouldBe` Just "deployed billing: Staging x2"

    it "answers a tool that never asks in one round" $ do
      r <- request handlers "tools/call" formCaps
        ["name" .= ("status" :: Text), "arguments" .= object ["service" .= ("billing" :: Text)]]
      firstText r `shouldBe` Just "billing is up"

    it "falls back without a round trip when the client cannot ask" $ do
      r <- request handlers "tools/call" noCaps (callDeploy [])
      firstText r `shouldBe` Just "no target: Unavailable"

    it "still derives IO handlers" $ do
      r <- request noHandlers { tools = Just ioTools } "tools/call" formCaps (callDeploy [])
      firstText r `shouldBe` Just "deployed billing"

    it "asks from a handler with structured output" $ do
      let hs = noHandlers { tools = Just outputTools }
      r1 <- request hs "tools/call" formCaps (callDeploy [])
      fmap (at ["message"] . snd) (questionOf r1) `shouldBe` Just (Just "Go?")
      r2 <- request hs "tools/call" formCaps $ callDeploy $ answering r1 ["proceed" .= True]
      at ["result", "structuredContent"] r2 `shouldBe` Just (object ["deployed" .= True])

    it "asks from a derived prompt" $ do
      let get extra = request handlers "prompts/get" formCaps $
            ["name" .= ("briefing" :: Text), "arguments" .= object ["team" .= ("ops" :: Text)]] ++ extra
      r1 <- get []
      fmap (at ["message"] . snd) (questionOf r1) `shouldBe` Just (Just "Include incidents for ops?")
      r2 <- get (answering r1 ["proceed" .= True])
      firstText r2 `shouldBe` Just "briefing with incidents"

    it "asks from a derived resource" $ do
      let readIt extra = request handlers "resources/read" formCaps $
            ["uri" .= ("resource://secrets" :: Text)] ++ extra
      r1 <- readIt []
      fmap (at ["message"] . snd) (questionOf r1) `shouldBe` Just (Just "Reveal secrets?")
      r2 <- readIt (answering r1 ["proceed" .= False])
      firstText r2 `shouldBe` Just "hidden"
