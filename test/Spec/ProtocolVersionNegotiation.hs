{-# LANGUAGE OverloadedStrings #-}

module Spec.ProtocolVersionNegotiation (spec) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec

import MCP.Server.Handlers (handleInitialize)
import MCP.Server.JsonRpc (JsonRpcRequest(..), RequestId(..), JsonRpcResponse(..), JsonRpcError(..))
import MCP.Server.Protocol (protocolVersion)
import MCP.Server.Types (McpServerInfo(..))

-- | Test that server performs proper version negotiation according to MCP spec.
--
-- From the spec: "If the server supports the requested protocol version,
-- it MUST respond with the same version. Otherwise, the server MUST respond
-- with another protocol version it supports."
--
-- The wire format for the basic tool/resource/prompt operations this library
-- implements is unchanged across the date-versioned revisions 2024-11-05,
-- 2025-03-26, 2025-06-18 and 2025-11-25, so the server echoes back any of
-- these the client proposes. Unknown versions fall back to the server's own
-- 'protocolVersion'.
spec :: Spec
spec = describe "Protocol Version Negotiation" $ do
  let testServerInfo = McpServerInfo
        { serverName = "Test Server"
        , serverVersion = "1.0.0"
        , serverInstructions = "Test server for version negotiation"
        }

  -- Issue an initialize request proposing the given protocol version and
  -- return the negotiated version from the server's (non-error) response.
  let negotiate :: Text -> IO Text
      negotiate clientVersion = do
        let params = object
              [ "protocolVersion" .= String clientVersion
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
        case responseError response of
          Just err ->
            error $ "Server returned error instead of negotiating version. "
                 ++ "Per MCP spec, server MUST respond with a supported version, "
                 ++ "not an error. Error was: " ++ show (errorMessage err)
          Nothing -> case responseResult response of
            Nothing -> error "Response has no result"
            Just (Object result) -> case KM.lookup "protocolVersion" result of
              Just (String version) -> pure version
              Just other -> error $ "protocolVersion is not a string: " ++ show other
              Nothing -> error "Response missing protocolVersion"
            Just other -> error $ "Result is not an object: " ++ show other

  it "echoes back the newest revision (2025-11-25) when the client proposes it" $
    negotiate "2025-11-25" `shouldReturn` "2025-11-25"

  it "echoes back 2025-06-18 when the client proposes it" $
    negotiate "2025-06-18" `shouldReturn` "2025-06-18"

  it "echoes back 2025-03-26 when the client proposes it" $
    negotiate "2025-03-26" `shouldReturn` "2025-03-26"

  -- Regression: Claude Code proposes 2024-11-05 and disconnects if it does not
  -- receive that exact version back, so tools never appear.
  it "echoes back the oldest compatible revision (2024-11-05) when the client proposes it" $
    negotiate "2024-11-05" `shouldReturn` "2024-11-05"

  it "falls back to the server's own version for an unknown/unsupported version" $
    negotiate "1999-01-01" `shouldReturn` protocolVersion
