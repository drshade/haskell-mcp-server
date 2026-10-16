{-# LANGUAGE OverloadedStrings #-}

-- | Coverage for multi round-trip requests (ADR 0009, phase 1): the
-- request-state codec, and dispatch of 'InputRequired' outcomes — the
-- @input_required@ result, verified retries, and every way a retry's state
-- is rejected.
module Spec.InputRequired (spec) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap   as KM
import           Data.Aeson.Types    (Pair)
import qualified Data.ByteString     as BS
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           MCP.Server
import           MCP.Server.Handlers (handleMcpMessage)
import           MCP.Server.JsonRpc
import           Test.Hspec
import           TestHelpers         (testStateCodec)

-- | A tool that asks for confirmation, then reports what it was given.
confirmTool :: ToolCallHandler
confirmTool ctx _name args = pure $ Right $
  case Map.lookup "proceed" (clientInputResponses ctx) of
    Nothing -> InputRequired
      (Map.singleton "proceed" (ElicitForm ("Deploy " <> target <> "?") confirmSchema))
      (Just ("planned:" <> target))
    Just (ElicitResponse ElicitAccept (Just content)) -> Complete $ toolResult
      [ ContentText $ "state=" <> fromMaybe "none" (clientRequestState ctx)
          <> " answer=" <> T.pack (show (Map.lookup "ok" content))
      ]
    Just other -> Complete $ toolError ("not confirmed: " <> T.pack (show other))
  where
    target = case Map.lookup "target" args of
      Just (String t) -> t
      _               -> "nowhere"

-- | A tool that sends the user to a URL.
linkTool :: ToolCallHandler
linkTool _ _ _ = pure $ Right $
  InputRequired (Map.singleton "login" (ElicitUrl "Sign in" loginUri)) Nothing
  where
    loginUri = fromMaybe (error "bad URI") (parseURI "https://example.com/login")

confirmSchema :: Schema
confirmSchema = schema $ SchemaObject [("ok", schema SchemaBoolean)] ["ok"]

handlers :: McpServerHandlers
handlers = noHandlers
  { tools = Just
      ( \_ -> pure []
      , \ctx name args -> case name of
          "link" -> linkTool ctx name args
          _      -> confirmTool ctx name args
      )
  , resources = Just
      ( \_ -> pure []
      , \ctx uri -> pure $ Right $
          if Map.null (clientInputResponses ctx)
            then InputRequired (Map.singleton "q" (ElicitForm "Which?" confirmSchema)) Nothing
            else Complete (ResourceText uri "text/plain" "read after asking")
      )
  , prompts = Just
      ( \_ -> pure []
      , \ctx _ _ -> pure $ Right $
          if Map.null (clientInputResponses ctx)
            then InputRequired (Map.singleton "q" (ElicitForm "Tone?" confirmSchema)) (Just "p")
            else Complete (PromptResult Nothing [PromptMessage RoleUser (ContentText "asked")])
      )
  }

serverInfo :: McpServerInfo
serverInfo = McpServerInfo "T" "1" ""

-- | Dispatch one request through the given codec and context.
runWith :: StateCodec -> ClientContext -> Text -> Value -> IO Value
runWith codec ctx method params = do
  response <- handleMcpMessage serverInfo defaultCacheHints codec noNotificationSupport
    (\_ -> pure ()) handlers ctx $ JsonRpcMessageRequest JsonRpcRequest
      { requestJsonrpc = "2.0"
      , requestId = RequestIdNumber 1
      , requestMethod = method
      , requestParams = Just params
      }
  case response of
    Just (JsonRpcMessageResponse r) -> pure (toJSON r)
    other -> fail ("expected a response, got " ++ show other)

run :: Text -> Value -> IO Value
run = runWith testStateCodec anonymousContext

-- | Modern params with the given client capabilities.
withCaps :: Value -> [Pair] -> Value
withCaps caps extra = object $ extra ++
  [ "_meta" .= object
      [ "io.modelcontextprotocol/protocolVersion" .= ("2026-07-28" :: Text)
      , "io.modelcontextprotocol/clientCapabilities" .= caps
      ]
  ]

formCaps, urlCaps, noCaps :: Value
formCaps = object ["elicitation" .= object ["form" .= object []]]
urlCaps = object ["elicitation" .= object ["url" .= object []]]
noCaps = object []

callParams :: Value -> Text -> [Pair] -> Value
callParams caps target extra = withCaps caps $
  [ "name" .= ("deploy" :: Text)
  , "arguments" .= object ["target" .= target]
  ] ++ extra

at :: [Key] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup k o >>= at ks
at _ _ = Nothing

stateOf :: Value -> Text
stateOf v = case at ["result", "requestState"] v of
  Just (String s) -> s
  _               -> error ("no requestState in " ++ show v)

errorOf :: Value -> Maybe (Int, Text)
errorOf v = do
  Number code <- at ["error", "code"] v
  String msg <- at ["error", "message"] v
  pure (round code, msg)

accept :: Value
accept = object ["proceed" .= object ["action" .= ("accept" :: Text), "content" .= object ["ok" .= True]]]

-- | The first round, then the retry with the given answers and state.
retry :: Value -> Text -> [Pair] -> IO Value
retry caps target extra = run "tools/call" (callParams caps target extra)

spec :: Spec
spec = describe "Multi round-trip requests" $ do
  describe "The AEAD state codec" $ do
    let key kid n = fromMaybe (error "bad key") (stateKey kid (BS.replicate 32 n))
        keyA = key "a" 1
        keyB = key "b" 2

    it "opens what it sealed" $ do
      let codec = aeadStateCodec keyA []
      sealed <- sealState codec "hello"
      openState codec sealed `shouldReturn` Right "hello"

    it "uses a fresh nonce for every state" $ do
      let codec = aeadStateCodec keyA []
      s1 <- sealState codec "same"
      s2 <- sealState codec "same"
      s1 `shouldNotBe` s2

    it "rejects a state altered after sealing" $ do
      let codec = aeadStateCodec keyA []
      sealed <- sealState codec "hello"
      -- Change one base64 character well inside the payload
      let (prefix, rest) = T.splitAt (T.length (fst (T.breakOn "." sealed)) + 10) sealed
          swapped = if T.head rest == 'A' then 'B' else 'A'
          tampered = prefix <> T.cons swapped (T.tail rest)
      openState codec tampered `shouldReturn` Left StateInvalid

    it "reports a state sealed under a key it does not have" $ do
      sealed <- sealState (aeadStateCodec keyA []) "hello"
      openState (aeadStateCodec keyB []) sealed `shouldReturn` Left StateUnknownKey

    it "opens states sealed under a retired key during rotation" $ do
      sealed <- sealState (aeadStateCodec keyA []) "hello"
      openState (aeadStateCodec keyB [keyA]) sealed `shouldReturn` Right "hello"

    it "gives each ephemeral codec its own key" $ do
      c1 <- ephemeralStateCodec
      c2 <- ephemeralStateCodec
      sealed <- sealState c1 "hello"
      openState c1 sealed `shouldReturn` Right "hello"
      openState c2 sealed `shouldReturn` Left StateUnknownKey

    it "validates keys and key ids" $ do
      fmap stateKeyId (stateKey "k-1_x" (BS.replicate 32 0)) `shouldBe` Just "k-1_x"
      fmap stateKeyId (stateKey "k" (BS.replicate 31 0)) `shouldBe` Nothing
      fmap stateKeyId (stateKey "has.dot" (BS.replicate 32 0)) `shouldBe` Nothing
      fmap stateKeyId (stateKey "" (BS.replicate 32 0)) `shouldBe` Nothing

  describe "clientSupportsInput" $ do
    let modern caps = anonymousContext
          { clientProtocolVersion = Just "2026-07-28", clientCapabilities = Just caps }
        form = ElicitForm "?" confirmSchema
        url = ElicitUrl "?" (fromMaybe (error "bad URI") (parseURI "https://example.com"))
    it "requires a modern client" $
      clientSupportsInput anonymousContext { clientCapabilities = Just formCaps } form `shouldBe` False
    it "treats an empty elicitation capability as form only" $ do
      let ctx = modern (object ["elicitation" .= object []])
      clientSupportsInput ctx form `shouldBe` True
      clientSupportsInput ctx url `shouldBe` False
    it "checks each elicitation mode" $ do
      clientSupportsInput (modern urlCaps) url `shouldBe` True
      clientSupportsInput (modern urlCaps) form `shouldBe` False
    it "checks sampling and roots, and refuses other methods" $ do
      let ctx = modern (object ["sampling" .= object [], "roots" .= object []])
      clientSupportsInput ctx (RawInputRequest "sampling/createMessage" Null) `shouldBe` True
      clientSupportsInput ctx (RawInputRequest "roots/list" Null) `shouldBe` True
      clientSupportsInput ctx (RawInputRequest "tools/call" Null) `shouldBe` False
      clientSupportsInput (modern noCaps) (RawInputRequest "roots/list" Null) `shouldBe` False

  describe "The first round" $ do
    it "answers with an input_required result" $ do
      v <- run "tools/call" (callParams formCaps "prod" [])
      at ["result", "resultType"] v `shouldBe` Just (String "input_required")
      at ["result", "inputRequests", "proceed"] v `shouldBe` Just (object
        [ "method" .= ("elicitation/create" :: Text)
        , "params" .= object
            [ "mode" .= ("form" :: Text)
            , "message" .= ("Deploy prod?" :: Text)
            , "requestedSchema" .= confirmSchema
            ]
        ])
      stateOf v `shouldSatisfy` T.isPrefixOf "test."
      at ["result", "_meta", "io.modelcontextprotocol/serverInfo", "name"] v
        `shouldBe` Just (String "T")

    it "does not reveal the handler's state" $ do
      v <- run "tools/call" (callParams formCaps "prod" [])
      stateOf v `shouldNotSatisfy` T.isInfixOf "planned"

    it "sends URL-mode elicitation" $ do
      v <- run "tools/call" (withCaps urlCaps ["name" .= ("link" :: Text)])
      at ["result", "inputRequests", "login", "params"] v `shouldBe` Just (object
        [ "mode" .= ("url" :: Text)
        , "message" .= ("Sign in" :: Text)
        , "url" .= ("https://example.com/login" :: Text)
        ])

    it "omits cacheability fields from input_required results" $ do
      v <- run "resources/read" (withCaps formCaps ["uri" .= ("resource://x" :: Text)])
      at ["result", "resultType"] v `shouldBe` Just (String "input_required")
      at ["result", "ttlMs"] v `shouldBe` Nothing
      at ["result", "cacheScope"] v `shouldBe` Nothing

    it "works for prompts/get" $ do
      v <- run "prompts/get" (withCaps formCaps ["name" .= ("p" :: Text)])
      at ["result", "resultType"] v `shouldBe` Just (String "input_required")

  describe "The retry" $ do
    it "completes with the answers and the handler's own state" $ do
      first <- run "tools/call" (callParams formCaps "prod" [])
      v <- retry formCaps "prod" ["inputResponses" .= accept, "requestState" .= stateOf first]
      at ["result", "resultType"] v `shouldBe` Just (String "complete")
      at ["result", "content"] v `shouldBe` Just (toJSON
        [ContentText "state=planned:prod answer=Just (Bool True)"])

    it "passes declined answers to the handler" $ do
      first <- run "tools/call" (callParams formCaps "prod" [])
      v <- retry formCaps "prod"
        [ "inputResponses" .= object ["proceed" .= object ["action" .= ("decline" :: Text)]]
        , "requestState" .= stateOf first
        ]
      at ["result", "isError"] v `shouldBe` Just (Bool True)

    it "completes a resource read after asking" $ do
      first <- run "resources/read" (withCaps formCaps ["uri" .= ("resource://x" :: Text)])
      v <- run "resources/read" $ withCaps formCaps
        [ "uri" .= ("resource://x" :: Text)
        , "inputResponses" .= object ["q" .= object ["action" .= ("accept" :: Text), "content" .= object ["ok" .= True]]]
        , "requestState" .= stateOf first
        ]
      at ["result", "resultType"] v `shouldBe` Just (String "complete")
      at ["result", "ttlMs"] v `shouldSatisfy` (/= Nothing)

  describe "Rejected state" $ do
    it "rejects a state presented with different arguments" $ do
      first <- run "tools/call" (callParams formCaps "prod" [])
      v <- retry formCaps "staging" ["inputResponses" .= accept, "requestState" .= stateOf first]
      errorOf v `shouldBe` Just (-32602, "Invalid requestState")

    it "rejects a state presented on another method" $ do
      first <- run "prompts/get" (withCaps formCaps ["name" .= ("p" :: Text)])
      v <- run "resources/read" (withCaps formCaps ["uri" .= ("resource://x" :: Text), "requestState" .= stateOf first])
      errorOf v `shouldBe` Just (-32602, "Invalid requestState")

    it "rejects a state presented by another principal" $ do
      let alice = anonymousContext { clientPrincipal = Just (String "alice") }
          bob = anonymousContext { clientPrincipal = Just (String "bob") }
      first <- runWith testStateCodec alice "tools/call" (callParams formCaps "prod" [])
      v <- runWith testStateCodec bob "tools/call"
        (callParams formCaps "prod" ["inputResponses" .= accept, "requestState" .= stateOf first])
      errorOf v `shouldBe` Just (-32602, "Invalid requestState")
      ok <- runWith testStateCodec alice "tools/call"
        (callParams formCaps "prod" ["inputResponses" .= accept, "requestState" .= stateOf first])
      at ["result", "resultType"] ok `shouldBe` Just (String "complete")

    it "rejects an expired state" $ do
      let shortLived = testStateCodec { stateLifetime = -1 }
      first <- runWith shortLived anonymousContext "tools/call" (callParams formCaps "prod" [])
      v <- runWith shortLived anonymousContext "tools/call"
        (callParams formCaps "prod" ["inputResponses" .= accept, "requestState" .= stateOf first])
      fmap fst (errorOf v) `shouldBe` Just (-32602)
      fmap snd (errorOf v) `shouldSatisfy` maybe False (T.isInfixOf "expired")

    it "explains a state from another instance" $ do
      other <- ephemeralStateCodec
      first <- runWith other anonymousContext "tools/call" (callParams formCaps "prod" [])
      v <- retry formCaps "prod" ["inputResponses" .= accept, "requestState" .= stateOf first]
      fmap snd (errorOf v) `shouldSatisfy` maybe False (T.isInfixOf "another server instance")

    it "rejects a state that is not a string" $ do
      v <- retry formCaps "prod" ["requestState" .= (42 :: Int)]
      fmap fst (errorOf v) `shouldBe` Just (-32602)

    it "rejects malformed inputResponses" $ do
      v <- retry formCaps "prod" ["inputResponses" .= object ["proceed" .= (1 :: Int)]]
      fmap fst (errorOf v) `shouldBe` Just (-32602)

  describe "Clients that cannot answer" $ do
    it "is a server error to ask a client without the capability" $ do
      v <- run "tools/call" (callParams noCaps "prod" [])
      fmap fst (errorOf v) `shouldBe` Just (-32603)
      fmap snd (errorOf v) `shouldSatisfy` maybe False (T.isInfixOf "proceed")

    it "is a server error to ask for a mode the client lacks" $ do
      v <- run "tools/call" (withCaps formCaps ["name" .= ("link" :: Text)])
      fmap fst (errorOf v) `shouldBe` Just (-32603)

    it "is a server error to ask a legacy client" $ do
      v <- run "tools/call" (object ["name" .= ("deploy" :: Text)])
      fmap fst (errorOf v) `shouldBe` Just (-32603)

    it "ignores round fields on legacy requests" $ do
      v <- run "tools/call" (object
        [ "name" .= ("deploy" :: Text)
        , "inputResponses" .= accept
        , "requestState" .= ("anything" :: Text)
        ])
      -- The handler saw no answers, so it asked, which legacy cannot carry.
      fmap fst (errorOf v) `shouldBe` Just (-32603)
