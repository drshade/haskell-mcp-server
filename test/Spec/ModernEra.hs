{-# LANGUAGE OverloadedStrings #-}

-- | Coverage for dual-era operation: modern (2026-07-28, per-request _meta)
-- requests get server/discover, the modern result envelope and header
-- validation; legacy (initialize-handshake) requests are served byte-
-- identically to before.
module Spec.ModernEra (spec) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import MCP.Server
import MCP.Server.Handlers (handleMcpMessage)
import MCP.Server.JsonRpc
import MCP.Server.Transport.Http (BodyPeek (..), decodeSentinel, peekBody,
                                  peekIsModern, validateRequestHeaders)
import qualified Network.Wai as Wai
import Test.Hspec

testServerInfo :: McpServerInfo
testServerInfo = McpServerInfo
  { serverName = "Test Server"
  , serverVersion = "9.9.9"
  , serverInstructions = "Use the tools."
  }

-- A server that only provides tools; the call handler reports the protocol
-- version it saw in the ClientContext.
testHandlers :: McpServerHandlers
testHandlers = McpServerHandlers
  { prompts = Nothing
  , resources = Nothing
  , tools = Just
      ( \_ctx -> pure []
      , \ctx _name _args -> pure $ Right $ toToolResult
          ("version=" <> maybe "none" id (clientProtocolVersion ctx))
      )
  }

run :: JsonRpcMessage -> IO (Maybe JsonRpcMessage)
run = handleMcpMessage testServerInfo defaultCacheHints testHandlers anonymousContext

request :: Text -> Maybe Value -> JsonRpcMessage
request method params = JsonRpcMessageRequest $ JsonRpcRequest
  { requestJsonrpc = "2.0"
  , requestId = RequestIdNumber 1
  , requestMethod = method
  , requestParams = params
  }

-- params carrying the standard modern _meta
modernParams :: [Pair] -> Value
modernParams extra = object $ extra ++
  [ "_meta" .= object
      [ "io.modelcontextprotocol/protocolVersion" .= ("2026-07-28" :: Text)
      , "io.modelcontextprotocol/clientInfo" .= object ["name" .= ("test-client" :: Text)]
      , "io.modelcontextprotocol/clientCapabilities" .= object []
      ]
  ]

resultObject :: Maybe JsonRpcMessage -> IO Object
resultObject (Just (JsonRpcMessageResponse r)) = case responseResult r of
  Just (Object o) -> pure o
  other -> expectationFailure ("Expected object result, got: " ++ show other) >> pure KM.empty
resultObject other =
  expectationFailure ("Expected a response, got: " ++ show other) >> pure KM.empty

errorOf :: Maybe JsonRpcMessage -> IO JsonRpcError
errorOf (Just (JsonRpcMessageResponse r)) = case responseError r of
  Just e -> pure e
  Nothing -> expectationFailure "Expected an error response" >> pure (JsonRpcError 0 "" Nothing)
errorOf other =
  expectationFailure ("Expected a response, got: " ++ show other) >> pure (JsonRpcError 0 "" Nothing)

spec :: Spec
spec = describe "Dual-era protocol support" $ do

  describe "server/discover" $ do
    it "reports all supported revisions, newest first" $ do
      o <- resultObject =<< run (request "server/discover" (Just (modernParams [])))
      KM.lookup "supportedVersions" o `shouldBe`
        Just (toJSON (["2026-07-28", "2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05"] :: [Text]))

    it "advertises only capabilities with handlers, plus identity and instructions" $ do
      o <- resultObject =<< run (request "server/discover" (Just (modernParams [])))
      case KM.lookup "capabilities" o of
        Just (Object caps) -> do
          KM.member "tools" caps `shouldBe` True
          KM.member "prompts" caps `shouldBe` False
        other -> expectationFailure $ "capabilities not an object: " ++ show other
      KM.lookup "instructions" o `shouldBe` Just (String "Use the tools.")
      case KM.lookup "_meta" o of
        Just (Object m) ->
          KM.lookup "io.modelcontextprotocol/serverInfo" m `shouldBe`
            Just (object ["name" .= ("Test Server" :: Text), "version" .= ("9.9.9" :: Text)])
        other -> expectationFailure $ "_meta not an object: " ++ show other

    it "carries the modern result envelope even without _meta (probe)" $ do
      o <- resultObject =<< run (request "server/discover" Nothing)
      KM.lookup "resultType" o `shouldBe` Just (String "complete")
      KM.member "ttlMs" o `shouldBe` True
      KM.lookup "cacheScope" o `shouldBe` Just (String "private")

  describe "Modern requests" $ do
    it "stamps resultType and cache fields on cacheable results" $ do
      o <- resultObject =<< run (request "tools/list" (Just (modernParams [])))
      KM.lookup "resultType" o `shouldBe` Just (String "complete")
      KM.lookup "ttlMs" o `shouldBe` Just (Number 0)
      KM.lookup "cacheScope" o `shouldBe` Just (String "private")

    it "stamps resultType but not cache fields on tools/call" $ do
      o <- resultObject =<< run (request "tools/call"
        (Just (modernParams ["name" .= ("t" :: Text), "arguments" .= object []])))
      KM.lookup "resultType" o `shouldBe` Just (String "complete")
      KM.member "ttlMs" o `shouldBe` False

    it "exposes the declared protocol version to handlers via ClientContext" $ do
      o <- resultObject =<< run (request "tools/call"
        (Just (modernParams ["name" .= ("t" :: Text), "arguments" .= object []])))
      KM.lookup "content" o `shouldBe`
        Just (toJSON [object ["type" .= ("text" :: Text), "text" .= ("version=2026-07-28" :: Text)]])

    it "rejects undeclared revisions with UnsupportedProtocolVersionError" $ do
      e <- errorOf =<< run (request "tools/list" (Just (object
        [ "_meta" .= object ["io.modelcontextprotocol/protocolVersion" .= ("2099-01-01" :: Text)] ])))
      errorCode e `shouldBe` (-32022)
      case errorData e of
        Just (Object d) -> do
          KM.lookup "requested" d `shouldBe` Just (String "2099-01-01")
          case KM.lookup "supported" d of
            Just (Array _) -> pure ()
            other -> expectationFailure $ "supported not a list: " ++ show other
        other -> expectationFailure $ "no error data: " ++ show other

  describe "Legacy requests" $ do
    it "are served without the modern envelope" $ do
      o <- resultObject =<< run (request "tools/list" Nothing)
      KM.member "resultType" o `shouldBe` False
      KM.member "ttlMs" o `shouldBe` False
      KM.member "_meta" o `shouldBe` False

    it "negotiate down to the newest legacy revision when proposing 2026-07-28 via initialize" $ do
      o <- resultObject =<< run (request "initialize" (Just (object
        [ "protocolVersion" .= ("2026-07-28" :: Text)
        , "capabilities" .= object []
        , "clientInfo" .= object ["name" .= ("c" :: Text), "version" .= ("1" :: Text)]
        ])))
      KM.lookup "protocolVersion" o `shouldBe` Just (String "2025-11-25")

  describe "HTTP request metadata validation" $ do
    let modernBody name = encode $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= (7 :: Int)
          , "method" .= ("tools/call" :: Text)
          , "params" .= modernParams ["name" .= name, "arguments" .= object []]
          ]
        reqWith hs = Wai.defaultRequest { Wai.requestHeaders = hs }

    it "peeks method, name, version and id from the body" $ do
      let peek = peekBody (modernBody ("get_weather" :: Text))
      peekMethod peek `shouldBe` Just "tools/call"
      peekName peek `shouldBe` Just "get_weather"
      peekMetaVersion peek `shouldBe` Just "2026-07-28"
      peekId peek `shouldBe` RequestIdNumber 7
      peekIsModern peek `shouldBe` True

    it "accepts a fully consistent modern request" $ do
      let peek = peekBody (modernBody ("get_weather" :: Text))
      validateRequestHeaders (reqWith
        [ ("MCP-Protocol-Version", "2026-07-28")
        , ("Mcp-Method", "tools/call")
        , ("Mcp-Name", "get_weather")
        ]) peek `shouldBe` Nothing

    it "rejects a missing Mcp-Method header with HeaderMismatch" $ do
      let peek = peekBody (modernBody ("get_weather" :: Text))
      fmap errorCode (validateRequestHeaders (reqWith
        [ ("MCP-Protocol-Version", "2026-07-28")
        , ("Mcp-Name", "get_weather")
        ]) peek) `shouldBe` Just (-32020)

    it "rejects a header/body protocol version mismatch with HeaderMismatch" $ do
      let peek = peekBody (modernBody ("get_weather" :: Text))
      fmap errorCode (validateRequestHeaders (reqWith
        [ ("MCP-Protocol-Version", "2025-11-25")
        , ("Mcp-Method", "tools/call")
        , ("Mcp-Name", "get_weather")
        ]) peek) `shouldBe` Just (-32020)

    it "accepts a base64-sentinel Mcp-Name for non-ASCII names" $ do
      let peek = peekBody (modernBody ("Hello, 世界" :: Text))
      validateRequestHeaders (reqWith
        [ ("MCP-Protocol-Version", "2026-07-28")
        , ("Mcp-Method", "tools/call")
        , ("Mcp-Name", "=?base64?SGVsbG8sIOS4lueVjA==?=")
        ]) peek `shouldBe` Nothing

    it "legacy bodies keep the relaxed rules (no headers required)" $ do
      let legacyBody = encode $ object
            [ "jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int)
            , "method" .= ("tools/list" :: Text) ]
      validateRequestHeaders (reqWith []) (peekBody legacyBody) `shouldBe` Nothing

    it "legacy bodies with an unsupported version header are rejected" $ do
      let legacyBody = encode $ object
            [ "jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int)
            , "method" .= ("tools/list" :: Text) ]
      fmap errorCode (validateRequestHeaders
        (reqWith [("MCP-Protocol-Version", "1999-01-01")])
        (peekBody legacyBody)) `shouldBe` Just (-32600)

  describe "Sentinel decoding" $ do
    it "passes plain values through" $
      decodeSentinel "us-west1" `shouldBe` "us-west1"
    it "decodes base64 sentinel values" $
      decodeSentinel "=?base64?SGVsbG8sIOS4lueVjA==?=" `shouldBe` "Hello, 世界"
