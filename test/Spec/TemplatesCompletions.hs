{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Coverage for resource templates (record constructors → URI templates)
-- and argument completion (completion/complete).
module Spec.TemplatesCompletions (spec) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server
import MCP.Server.Derive
import MCP.Server.Handlers (handleMcpMessage)
import MCP.Server.JsonRpc
import Test.Hspec
import TestTypes

templResourceHandlers :: (ResourceListHandler, ResourceReadHandler)
templResourceHandlers = $(deriveResourceHandler ''TemplResource 'handleTemplResource)

templTemplates :: ResourceTemplateListHandler
templTemplates = $(deriveResourceTemplatesWithDescription ''TemplResource
  [("MemberProfile", "A member's profile"), ("OrderItem", "One item of an order")])

readUri :: String -> IO (Either Error ResourceContent)
readUri s = case parseURI s of
  Just uri -> snd templResourceHandlers anonCtx uri
  Nothing  -> fail ("bad test URI: " ++ s)

expectText :: Either Error ResourceContent -> Text -> Expectation
expectText (Right (ResourceText _ _ content)) expected = content `shouldBe` expected
expectText other expected =
  expectationFailure $ "Expected ResourceText '" ++ T.unpack expected ++ "' but got: " ++ show other

-- A completion handler that records what it receives in its suggestions
testCompletions :: CompletionHandler
testCompletions _ ref argName partial ctxArgs = pure $ Right $ completionResult $
  case ref of
    CompletionRefPrompt name ->
      [ "prompt:" <> name <> ":" <> argName <> ":" <> partial
        <> ":" <> T.intercalate "," (Map.keys ctxArgs)
      ]
    CompletionRefResource uri -> [ "resource:" <> uri <> ":" <> argName ]

completionServer :: McpServerHandlers
completionServer = noHandlers { completions = Just testCompletions }

runReq :: McpServerHandlers -> Text -> Maybe Value -> IO (Maybe JsonRpcMessage)
runReq handlers method params =
  handleMcpMessage
    (McpServerInfo "T" "1" "")
    defaultCacheHints
    handlers
    anonymousContext
    (JsonRpcMessageRequest (JsonRpcRequest "2.0" (RequestIdNumber 1) method params))

resultOf :: Maybe JsonRpcMessage -> IO Object
resultOf (Just (JsonRpcMessageResponse r)) = case responseResult r of
  Just (Object o) -> pure o
  other -> expectationFailure ("Expected object result, got: " ++ show other) >> pure KM.empty
resultOf other =
  expectationFailure ("Expected a response, got: " ++ show other) >> pure KM.empty

spec :: Spec
spec = describe "Resource templates and completions" $ do

  describe "Template derivation" $ do
    it "advertises record constructors as URI templates" $ do
      templates <- templTemplates anonCtx
      map resourceTemplateURITemplate templates `shouldBe`
        [ "resource://member_profile/{memberId}"
        , "resource://order_item/{orderId}/{itemName}"
        ]
      map resourceTemplateDescription templates `shouldBe`
        [ Just "A member's profile", Just "One item of an order" ]

    it "excludes record constructors from the static resource list" $ do
      statics <- fst templResourceHandlers anonCtx
      map resourceDefinitionURI statics `shouldBe` ["resource://catalog"]

  describe "Template reads" $ do
    it "still reads static resources" $ do
      result <- readUri "resource://catalog"
      result `expectText` "The catalog"

    it "reads a single-parameter template" $ do
      result <- readUri "resource://member_profile/alice"
      result `expectText` "Profile of alice"

    it "percent-decodes template segments" $ do
      result <- readUri "resource://member_profile/alice%20smith"
      result `expectText` "Profile of alice smith"

    it "reads a multi-parameter template with typed segments" $ do
      result <- readUri "resource://order_item/42/widget"
      result `expectText` "Order 42 item widget"

    it "rejects segments that fail the field's type" $ do
      result <- readUri "resource://order_item/notanum/widget"
      case result of
        Left (InvalidParams msg) -> msg `shouldSatisfy` T.isInfixOf "template field 'orderId'"
        other -> expectationFailure $ "Expected InvalidParams but got: " ++ show other

    it "falls through to ResourceNotFound on a segment-count mismatch" $ do
      result <- readUri "resource://member_profile/a/b"
      case result of
        Left (ResourceNotFound _) -> pure ()
        other -> expectationFailure $ "Expected ResourceNotFound but got: " ++ show other

  describe "resources/templates/list dispatch" $ do
    it "returns templates and carries the modern cache envelope" $ do
      let handlers = noHandlers { resourceTemplates = Just templTemplates }
      o <- resultOf =<< runReq handlers "resources/templates/list" (Just (object
        [ "_meta" .= object ["io.modelcontextprotocol/protocolVersion" .= ("2026-07-28" :: Text)] ]))
      KM.member "resourceTemplates" o `shouldBe` True
      KM.member "ttlMs" o `shouldBe` True
      KM.lookup "resultType" o `shouldBe` Just (String "complete")

    it "answers -32601 when no template handler is configured" $ do
      resp <- runReq noHandlers "resources/templates/list" Nothing
      case resp of
        Just (JsonRpcMessageResponse r) ->
          fmap errorCode (responseError r) `shouldBe` Just (-32601)
        other -> expectationFailure $ "Expected a response, got: " ++ show other

  describe "completion/complete" $ do
    it "dispatches prompt refs with context arguments" $ do
      o <- resultOf =<< runReq completionServer "completion/complete" (Just (object
        [ "ref" .= object ["type" .= ("ref/prompt" :: Text), "name" .= ("recipe" :: Text)]
        , "argument" .= object ["name" .= ("idea" :: Text), "value" .= ("pa" :: Text)]
        , "context" .= object ["arguments" .= object ["cuisine" .= ("italian" :: Text)]]
        ]))
      KM.lookup "completion" o `shouldBe`
        Just (object ["values" .= (["prompt:recipe:idea:pa:cuisine"] :: [Text])])

    it "dispatches resource template refs" $ do
      o <- resultOf =<< runReq completionServer "completion/complete" (Just (object
        [ "ref" .= object ["type" .= ("ref/resource" :: Text), "uri" .= ("resource://member_profile/{memberId}" :: Text)]
        , "argument" .= object ["name" .= ("memberId" :: Text), "value" .= ("al" :: Text)]
        ]))
      KM.lookup "completion" o `shouldBe`
        Just (object ["values" .= (["resource:resource://member_profile/{memberId}:memberId"] :: [Text])])

    it "answers -32601 when no completion handler is configured" $ do
      resp <- runReq noHandlers "completion/complete" Nothing
      case resp of
        Just (JsonRpcMessageResponse r) ->
          fmap errorCode (responseError r) `shouldBe` Just (-32601)
        other -> expectationFailure $ "Expected a response, got: " ++ show other

  describe "Capabilities" $ do
    it "advertises completions and resources-via-templates when configured" $ do
      let handlers = noHandlers
            { resourceTemplates = Just templTemplates
            , completions = Just testCompletions
            }
      o <- resultOf =<< runReq handlers "server/discover" Nothing
      case KM.lookup "capabilities" o of
        Just (Object caps) -> do
          KM.member "completions" caps `shouldBe` True
          KM.member "resources" caps `shouldBe` True
          KM.member "tools" caps `shouldBe` False
        other -> expectationFailure $ "capabilities not an object: " ++ show other