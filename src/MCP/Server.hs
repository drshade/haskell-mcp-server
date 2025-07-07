{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server
  ( -- * Server Runtime
    runMcpServerStdio
  , runMcpServerHttp
  , runMcpServerHttpWithConfig

    -- * Transport Configuration
  , HttpConfig(..)

    -- * Utility Functions
  , jsonValueToText

    -- * Re-exports
  , module MCP.Server.Types
  ) where

import           Data.Aeson
import           Data.Text              (Text)
import qualified Data.Text              as T

import           MCP.Server.Transport.Stdio (transportRunStdio)
import           MCP.Server.Transport.Http (HttpConfig(..), transportRunHttp, defaultHttpConfig)
import           MCP.Server.Types

-- | Convert JSON Value to Text representation suitable for handlers
jsonValueToText :: Value -> Text
jsonValueToText (String t) = t
jsonValueToText (Number n) = 
    -- Check if it's a whole number, if so format as integer
    if fromInteger (round n) == n
        then T.pack $ show (round n :: Integer)
        else T.pack $ show n
jsonValueToText (Bool True) = "true"
jsonValueToText (Bool False) = "false"
jsonValueToText Null = ""
jsonValueToText v = T.pack $ show v

-- | Run an MCP server using STDIO transport
runMcpServerStdio :: McpServerInfo -> McpServerHandlers IO -> IO ()
runMcpServerStdio serverInfo handlers = transportRunStdio serverInfo handlers

-- | Run an MCP server using HTTP transport with default configuration
runMcpServerHttp :: McpServerInfo -> McpServerHandlers IO -> IO ()
runMcpServerHttp serverInfo handlers = transportRunHttp defaultHttpConfig serverInfo handlers

-- | Run an MCP server using HTTP transport with custom configuration
runMcpServerHttpWithConfig :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> IO ()
runMcpServerHttpWithConfig config serverInfo handlers = transportRunHttp config serverInfo handlers

