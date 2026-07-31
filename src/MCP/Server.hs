{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server
  ( -- * Server Runtime
    runMcpServerStdio
  , runMcpServerHttp
  , runMcpServerHttpWithConfig

    -- * Transport Configuration
  , HttpConfig(..)

    -- * Re-exports
  , module MCP.Server.Types
  ) where

import           MCP.Server.Transport.Stdio (transportRunStdio)
import           MCP.Server.Transport.Http (HttpConfig(..), transportRunHttp, defaultHttpConfig)
import           MCP.Server.Types

-- | Run an MCP server using STDIO transport
runMcpServerStdio :: McpServerInfo -> McpServerHandlers -> IO ()
runMcpServerStdio = transportRunStdio

-- | Run an MCP server using HTTP transport with default configuration
runMcpServerHttp :: McpServerInfo -> McpServerHandlers -> IO ()
runMcpServerHttp = transportRunHttp defaultHttpConfig

-- | Run an MCP server using HTTP transport with custom configuration
runMcpServerHttpWithConfig :: HttpConfig -> McpServerInfo -> McpServerHandlers -> IO ()
runMcpServerHttpWithConfig = transportRunHttp
