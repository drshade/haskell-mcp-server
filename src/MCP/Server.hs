{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server
  ( -- * Server Runtime
    runMcpServerStdio
  , runMcpServerStdioWithConfig
  , runMcpServerHttp
  , runMcpServerHttpWithConfig

    -- * Transport Configuration
  , StdioConfig(..)
  , defaultStdioConfig
  , HttpConfig(..)
  , defaultHttpConfig
  , mcpApplication

    -- * Change Notifications
  , McpNotifier(..)
  , NotificationSource
  , newMcpNotifier

    -- * Re-exports
  , module MCP.Server.Types
  ) where

import           MCP.Server.Notifications (McpNotifier (..), NotificationSource,
                                           newMcpNotifier)
import           MCP.Server.Transport.Stdio (StdioConfig (..), defaultStdioConfig,
                                             transportRunStdio,
                                             transportRunStdioWithConfig)
import           MCP.Server.Transport.Http (HttpConfig(..), transportRunHttp, defaultHttpConfig,
                                            mcpApplication)
import           MCP.Server.Types

-- | Run an MCP server using STDIO transport
runMcpServerStdio :: McpServerInfo -> McpServerHandlers -> IO ()
runMcpServerStdio = transportRunStdio

-- | Run an MCP server using STDIO transport with custom configuration
runMcpServerStdioWithConfig :: StdioConfig -> McpServerInfo -> McpServerHandlers -> IO ()
runMcpServerStdioWithConfig = transportRunStdioWithConfig

-- | Run an MCP server using HTTP transport with default configuration
runMcpServerHttp :: McpServerInfo -> McpServerHandlers -> IO ()
runMcpServerHttp = transportRunHttp defaultHttpConfig

-- | Run an MCP server using HTTP transport with custom configuration
runMcpServerHttpWithConfig :: HttpConfig -> McpServerInfo -> McpServerHandlers -> IO ()
runMcpServerHttpWithConfig = transportRunHttp
