{-# LANGUAGE OverloadedStrings #-}

module Spec.ProtocolVersionNegotiation (spec) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec

import MCP.Server.Handlers (handleInitialize)
import MCP.Server.JsonRpc (JsonRpcRequest(..), RequestId(..), JsonRpcResponse(..), JsonRpcError(..))
import MCP.Server.Types (McpServerInfo(..))

-- | Test that server performs proper version negotiation according to MCP spec
--
-- From the spec: "If the server supports the requested protocol version,
-- it MUST respond with the same version. Otherwise, the server MUST respond
-- with another protocol version it supports."
--
-- This means if a client sends protocolVersion "2025-11-25" and the server
-- only supports "2025-06-18", the server should respond with:
-- {
--   "jsonrpc": "2.0",
--   "id": 0,
--   "result": {
--     "protocolVersion": "2025-06-18",
--     ...
--   }
-- }
--
-- NOT with an error like: {"jsonrpc":"2.0","id":0,"error":{"code":-32602,...}}
spec :: Spec
spec = describe "Protocol Version Negotiation" $ do
  let testServerInfo = McpServerInfo
        { serverName = "Test Server"
        , serverVersion = "1.0.0"
        , serverInstructions = "Test server for version negotiation"
        }

  it "Server should respond with supported version when client requests unsupported version" $ do
    -- Create an initialize request with a newer protocol version
    -- that the library doesn't support
    let params = object
          [ "protocolVersion" .= String "2025-11-25"  -- Newer than library supports
          , "capabilities" .= object []
          , "clientInfo" .= object
              [ "name" .= String "test-client"
              , "version" .= String "1.0.0"
              ]
          ]
        request = JsonRpcRequest
          { requestJsonrpc = "2.0"
          , requestId = RequestIdNumber 0
          , requestMethod = "initialize"
          , requestParams = Just params
          }

    -- Call handleInitialize directly (it runs in IO monad)
    response <- handleInitialize testServerInfo request

    -- Check if it's an error response
    case responseError response of
      Just err -> do
        -- This is the bug! Server returned an error instead of negotiating
        expectationFailure $
          "Server returned error instead of negotiating version. " ++
          "Per MCP spec, server MUST respond with a supported version, " ++
          "not an error. Error was: " ++ show (errorMessage err)
      Nothing -> do
        -- Good! Now verify the result has protocolVersion
        case responseResult response of
          Nothing -> expectationFailure "Response has no result"
          Just (Object result) -> do
            case KM.lookup "protocolVersion" result of
              Just (String version) -> do
                -- The server should have responded with its supported version
                -- We expect "2025-06-18" based on the library
                version `shouldBe` "2025-06-18"
              Just other -> expectationFailure $ "protocolVersion is not a string: " ++ show other
              Nothing -> expectationFailure "Response missing protocolVersion"
          Just other -> expectationFailure $ "Result is not an object: " ++ show other

  it "Server should respond with same version when client requests supported version" $ do
    -- This test verifies the happy path: client requests "2025-06-18"
    -- and server responds with "2025-06-18"
    let params = object
          [ "protocolVersion" .= String "2025-06-18"  -- Library's supported version
          , "capabilities" .= object []
          , "clientInfo" .= object
              [ "name" .= String "test-client"
              , "version" .= String "1.0.0"
              ]
          ]
        request = JsonRpcRequest
          { requestJsonrpc = "2.0"
          , requestId = RequestIdNumber 0
          , requestMethod = "initialize"
          , requestParams = Just params
          }

    response <- handleInitialize testServerInfo request

    -- Check it's not an error
    case responseError response of
      Just err -> expectationFailure $ "Unexpected error: " ++ show (errorMessage err)
      Nothing -> do
        -- Verify protocolVersion matches
        case responseResult response of
          Nothing -> expectationFailure "Response has no result"
          Just (Object result) -> do
            case KM.lookup "protocolVersion" result of
              Just (String version) -> version `shouldBe` "2025-06-18"
              Just other -> expectationFailure $ "protocolVersion is not a string: " ++ show other
              Nothing -> expectationFailure "Response missing protocolVersion"
          Just other -> expectationFailure $ "Result is not an object: " ++ show other
