{-# LANGUAGE OverloadedStrings #-}

-- | Drive the Streamable HTTP transport in-process through the exported
-- WAI application, without Warp or a socket. The handler-level specs pin
-- what 'handleMcpMessage' answers; these pin what actually reaches an
-- HTTP client, which is the layer that used to lose a throwing handler
-- (a bare Warp 500 for single-JSON responses, a dropped connection for
-- SSE ones).
module Spec.HttpTransport (spec) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import MCP.Server
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec
import TestHelpers (testStateCodec)
import qualified Data.Map as Map

server :: McpServerHandlers
server = noHandlers
  { tools = Just
      ( \_ -> pure []
      , \ctx name _ -> case name of
          "boom" -> throwIO (ErrorCall "tool exploded")
          "ok"   -> pure $ Right $ Complete $ toToolResult ("fine" :: Text)
          "ask"  -> pure $ Right $ case Map.lookup "q" (clientInputResponses ctx) of
            Nothing -> InputRequired
              (Map.singleton "q" (ElicitForm "Sure?" (schema (SchemaObject [] []))))
              (Just "asked")
            Just _ -> Complete $ toToolResult
              ("answered after " <> maybe "nothing" id (clientRequestState ctx))
          _      -> pure $ Left $ UnknownTool name
      )
  }

app :: IO Wai.Application
app = mcpApplication defaultHttpConfig
        { httpAllowedOrigins = Nothing
        , httpStateCodec = Just testStateCodec  -- shared across 'post's
        }
        (McpServerInfo "T" "1" "") server

-- | POST a JSON body to /mcp and collect the full response, whether it was
-- sent as a single body or streamed (SSE).
post :: [HTTP.Header] -> Value -> IO (HTTP.Status, [HTTP.Header], BS.ByteString)
post extraHeaders body = do
  chunks <- newIORef (BSL.toChunks (encode body))
  let nextChunk = atomicModifyIORef' chunks $ \cs -> case cs of
        []     -> ([], BS.empty)
        (c:cs') -> (cs', c)
      req = Wai.setRequestBodyChunks nextChunk Wai.defaultRequest
        { Wai.requestMethod = "POST"
        , Wai.rawPathInfo = "/mcp"
        , Wai.pathInfo = ["mcp"]
        , Wai.requestHeaders =
            ("Content-Type", "application/json")
              : ("Accept", "application/json, text/event-stream")
              : extraHeaders
        }
  out <- newIORef mempty
  result <- newIORef Nothing
  application <- app
  _ <- application req $ \resp -> do
    let (status, headers, withBody) = Wai.responseToStream resp
    withBody $ \streamingBody ->
      streamingBody (\b -> modifyIORef' out (<> b)) (pure ())
    writeIORef result (Just (status, headers))
    pure ResponseReceived
  Just (status, headers) <- readIORef result
  bytes <- BSL.toStrict . B.toLazyByteString <$> readIORef out
  pure (status, headers, bytes)

toolCall :: Text -> [(Key, Value)] -> Value
toolCall name meta = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= (7 :: Int)
  , "method" .= ("tools/call" :: Text)
  , "params" .= object
      ([ "name" .= name, "arguments" .= object [] ]
        ++ [ "_meta" .= object meta | not (null meta) ])
  ]

-- | The JSON-RPC error (code, message) in a response body, or in the
-- last SSE data event of a streamed one.
errorIn :: BS.ByteString -> Maybe (Int, Text)
errorIn raw = do
  let payload = case [ BS.drop 6 l | l <- BS.split 10 raw, "data: " `BS.isPrefixOf` l ] of
        [] -> raw
        ls -> last ls
  Object o <- decodeStrict payload
  Object e <- KM.lookup "error" o
  Number c <- KM.lookup "code" e
  String m <- KM.lookup "message" e
  pure (round c, m)

-- | A modern tools/call of the "ask" tool, with extra params.
askCall :: [(Key, Value)] -> Value
askCall extra = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= (8 :: Int)
  , "method" .= ("tools/call" :: Text)
  , "params" .= object (
      [ "name" .= ("ask" :: Text)
      , "_meta" .= object
          [ "io.modelcontextprotocol/protocolVersion" .= ("2026-07-28" :: Text)
          , "io.modelcontextprotocol/clientCapabilities" .= object ["elicitation" .= object []]
          ]
      ] ++ extra)
  ]

modernHeaders :: Text -> [HTTP.Header]
modernHeaders name =
  [ ("MCP-Protocol-Version", "2026-07-28")
  , ("Mcp-Method", "tools/call")
  , ("Mcp-Name", TE.encodeUtf8 name)
  ]

resultIn :: BS.ByteString -> Maybe Object
resultIn raw = do
  Object o <- decodeStrict raw
  Object r <- KM.lookup "result" o
  pure r

spec :: Spec
spec = describe "HTTP transport (in-process WAI)" $ do
  it "carries a multi round-trip request across two POSTs" $ do
    (status1, _, body1) <- post (modernHeaders "ask") (askCall [])
    HTTP.statusCode status1 `shouldBe` 200
    let first = resultIn body1
    (first >>= KM.lookup "resultType") `shouldBe` Just (String "input_required")
    Just (String sealed) <- pure (first >>= KM.lookup "requestState")
    (status2, _, body2) <- post (modernHeaders "ask") $ askCall
      [ "inputResponses" .= object ["q" .= object ["action" .= ("accept" :: Text), "content" .= object []]]
      , "requestState" .= sealed
      ]
    HTTP.statusCode status2 `shouldBe` 200
    body2 `shouldSatisfy` ("answered after asked" `BS.isInfixOf`)

  it "answers a normal tool call with 200 and a result" $ do
    (status, _, body) <- post [] (toolCall "ok" [])
    HTTP.statusCode status `shouldBe` 200
    body `shouldSatisfy` ("\"fine\"" `BS.isInfixOf`)

  it "answers a throwing handler with 200 and a -32603 body (legacy, single-JSON)" $ do
    (status, headers, body) <- post [] (toolCall "boom" [])
    HTTP.statusCode status `shouldBe` 200
    lookup "Content-Type" headers `shouldBe` Just "application/json"
    fmap fst (errorIn body) `shouldBe` Just (-32603)
    fmap snd (errorIn body) `shouldSatisfy` maybe False (T.isInfixOf "tool exploded")

  it "answers a throwing handler inside the SSE stream (legacy, progressToken)" $ do
    (status, headers, body) <- post [] (toolCall "boom" ["progressToken" .= ("t1" :: Text)])
    HTTP.statusCode status `shouldBe` 200
    lookup "Content-Type" headers `shouldBe` Just "text/event-stream"
    body `shouldSatisfy` ("data: " `BS.isPrefixOf`)
    fmap fst (errorIn body) `shouldBe` Just (-32603)

  it "answers a throwing handler with a -32603 body (modern 2026-07-28)" $ do
    (status, _, body) <- post
      [ ("MCP-Protocol-Version", "2026-07-28")
      , ("Mcp-Method", "tools/call")
      , ("Mcp-Name", "boom")
      ]
      (toolCall "boom" ["io.modelcontextprotocol/protocolVersion" .= ("2026-07-28" :: Text)])
    HTTP.statusCode status `shouldBe` 200
    fmap fst (errorIn body) `shouldBe` Just (-32603)
    -- an error carries no result envelope
    body `shouldSatisfy` (not . ("resultType" `BS.isInfixOf`))
