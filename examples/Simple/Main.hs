{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.IORef
import           MCP.Server
import           MCP.Server.Derive
import           System.IO         (hPutStrLn, stderr)
import           Types

main :: IO ()
main = do
    hPutStrLn stderr "Starting Simple MCP Server..."

    -- Create a simple in-memory store
    store <- newIORef []

    let handleTool :: ClientContext -> SimpleTool -> IO Content
        handleTool _ (GetValue k) = do
            pairs <- readIORef store
            case lookup k pairs of
                Nothing -> pure $ ContentText $ "Key '" <> k <> "' not found"
                Just v  -> pure $ ContentText v
        handleTool _ (SetValue k v) = do
            pairs <- readIORef store
            let newPairs = (k, v) : filter ((/= k) . fst) pairs
            writeIORef store newPairs
            pure $ ContentText $ "Set '" <> k <> "' to '" <> v <> "'"

    -- Derive the tool handlers using Template Haskell with descriptions
    let tools = $(deriveToolHandlerWithDescription ''SimpleTool 'handleTool simpleDescriptions)
     in runMcpServerStdio
        McpServerInfo
            { serverName = "Simple Key-Value MCP Server"
            , serverVersion = "1.0.0"
            , serverInstructions = "A simple key-value store with GetValue and SetValue tools"
            }
        McpServerHandlers
            { prompts = Nothing     -- No prompts in this example
            , resources = Nothing   -- No resources in this example
            , tools = Just tools    -- Only tools in this example
            }
