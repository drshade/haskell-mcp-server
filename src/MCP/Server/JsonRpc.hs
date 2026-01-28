{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.JsonRpc
  ( -- * JSON-RPC Types
    JsonRpcRequest(..)
  , JsonRpcResponse(..)
  , JsonRpcError(..)
  , JsonRpcNotification(..)
  , JsonRpcMessage(..)
  , RequestId(..)

    -- * JSON-RPC Functions
  , makeSuccessResponse
  , makeErrorResponse
  , makeNotification
  , parseJsonRpcMessage
  , encodeJsonRpcMessage
  ) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import GHC.Generics (Generic)
import Control.Applicative ((<|>))

-- | JSON-RPC request ID
data RequestId
  = RequestIdText Text
  | RequestIdNumber Int
  | RequestIdNull
  deriving (Show, Eq, Generic)

instance ToJSON RequestId where
  toJSON (RequestIdText t) = toJSON t
  toJSON (RequestIdNumber n) = toJSON n
  toJSON RequestIdNull = Null

instance FromJSON RequestId where
  parseJSON (String t) = return $ RequestIdText t
  parseJSON (Number n) = return $ RequestIdNumber (round n)
  parseJSON Null = return RequestIdNull
  parseJSON _ = fail "Invalid request ID"

-- | JSON-RPC request
data JsonRpcRequest = JsonRpcRequest
  { requestJsonrpc :: Text
  , requestId :: RequestId
  , requestMethod :: Text
  , requestParams :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON JsonRpcRequest where
  toJSON req = object $
    [ "jsonrpc" .= requestJsonrpc req
    , "id" .= requestId req
    , "method" .= requestMethod req
    ] ++ maybe [] (\p -> ["params" .= p]) (requestParams req)

instance FromJSON JsonRpcRequest where
  parseJSON = withObject "JsonRpcRequest" $ \o -> JsonRpcRequest
    <$> o .: "jsonrpc"
    <*> o .: "id"
    <*> o .: "method"
    <*> o .:? "params"

-- | JSON-RPC error
data JsonRpcError = JsonRpcError
  { errorCode :: Int
  , errorMessage :: Text
  , errorData :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON JsonRpcError where
  toJSON err = object $
    [ "code" .= errorCode err
    , "message" .= errorMessage err
    ] ++ maybe [] (\d -> ["data" .= d]) (errorData err)

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \o -> JsonRpcError
    <$> o .: "code"
    <*> o .: "message"
    <*> o .:? "data"

-- | JSON-RPC response
data JsonRpcResponse = JsonRpcResponse
  { responseJsonrpc :: Text
  , responseId :: RequestId
  , responseResult :: Maybe Value
  , responseError :: Maybe JsonRpcError
  } deriving (Show, Eq, Generic)

instance ToJSON JsonRpcResponse where
  toJSON resp = object $
    [ "jsonrpc" .= responseJsonrpc resp
    , "id" .= responseId resp
    ] ++
    maybe [] (\r -> ["result" .= r]) (responseResult resp) ++
    maybe [] (\e -> ["error" .= e]) (responseError resp)

instance FromJSON JsonRpcResponse where
  parseJSON = withObject "JsonRpcResponse" $ \o -> JsonRpcResponse
    <$> o .: "jsonrpc"
    <*> o .: "id"
    <*> o .:? "result"
    <*> o .:? "error"

-- | JSON-RPC notification
data JsonRpcNotification = JsonRpcNotification
  { notificationJsonrpc :: Text
  , notificationMethod :: Text
  , notificationParams :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON JsonRpcNotification where
  toJSON notif = object $
    [ "jsonrpc" .= notificationJsonrpc notif
    , "method" .= notificationMethod notif
    ] ++ maybe [] (\p -> ["params" .= p]) (notificationParams notif)

instance FromJSON JsonRpcNotification where
  parseJSON = withObject "JsonRpcNotification" $ \o -> JsonRpcNotification
    <$> o .: "jsonrpc"
    <*> o .: "method"
    <*> o .:? "params"

-- | Union type for all JSON-RPC messages
data JsonRpcMessage
  = JsonRpcMessageRequest JsonRpcRequest
  | JsonRpcMessageResponse JsonRpcResponse
  | JsonRpcMessageNotification JsonRpcNotification
  deriving (Show, Eq, Generic)

instance ToJSON JsonRpcMessage where
  toJSON (JsonRpcMessageRequest req) = toJSON req
  toJSON (JsonRpcMessageResponse resp) = toJSON resp
  toJSON (JsonRpcMessageNotification notif) = toJSON notif

instance FromJSON JsonRpcMessage where
  parseJSON v = parseRequest v <|> parseResponse v <|> parseNotification v
    where
      parseRequest = fmap JsonRpcMessageRequest . parseJSON
      parseResponse = fmap JsonRpcMessageResponse . parseJSON
      parseNotification = fmap JsonRpcMessageNotification . parseJSON

-- | Create a successful JSON-RPC response
makeSuccessResponse :: RequestId -> Value -> JsonRpcResponse
makeSuccessResponse reqId result = JsonRpcResponse
  { responseJsonrpc = "2.0"
  , responseId = reqId
  , responseResult = Just result
  , responseError = Nothing
  }

-- | Create an error JSON-RPC response
makeErrorResponse :: RequestId -> JsonRpcError -> JsonRpcResponse
makeErrorResponse reqId err = JsonRpcResponse
  { responseJsonrpc = "2.0"
  , responseId = reqId
  , responseResult = Nothing
  , responseError = Just err
  }

-- | Create a JSON-RPC notification
makeNotification :: Text -> Maybe Value -> JsonRpcNotification
makeNotification method params = JsonRpcNotification
  { notificationJsonrpc = "2.0"
  , notificationMethod = method
  , notificationParams = params
  }

-- | Parse a JSON-RPC message from bytes
parseJsonRpcMessage :: Value -> Either String JsonRpcMessage
parseJsonRpcMessage = parseEither parseJSON

-- | Encode a JSON-RPC message to bytes
encodeJsonRpcMessage :: JsonRpcMessage -> Value
encodeJsonRpcMessage = toJSON
