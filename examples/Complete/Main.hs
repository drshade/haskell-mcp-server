{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           MCP.Server
import           MCP.Server.Derive
import           System.IO (hSetEncoding, stderr, stdout, utf8)
import           Types

-- High-level handler functions

handlePrompt :: ClientContext -> MyPrompt -> IO Content
handlePrompt _ (Recipe idea) =
    pure $ ContentText $ "Recipe prompt for " <> idea <> ": Start by gathering fresh ingredients..."
handlePrompt _ (Shopping description) =
    pure $ ContentText $ "Shopping prompt for " <> description <> ": Create a detailed shopping list..."

handleResource :: ClientContext -> URI -> MyResource -> IO ResourceContent
handleResource _ uri ProductCategories =
    pure $ ResourceText uri "text/plain" "Fresh Produce, Dairy, Bakery, Meat & Seafood, Frozen Foods"
handleResource _ uri SaleItems =
    pure $ ResourceText uri "text/plain" "Organic Apples $2.99/lb, Free Range Eggs $4.50/dozen, Artisan Bread $3.25/loaf"
handleResource _ uri HeadlineBannerAd =
    pure $ ResourceText uri "text/plain" "🛒 Weekly Special: 20% off all organic produce! 🥕🥬🍎"

handleTool :: ClientContext -> MyTool -> IO Content
handleTool _ (SearchForProduct q category) =
    case category of
        Nothing -> pure $ ContentText $ "Search results for '" <> q <> "': Found 15 products across all categories"
        Just cat -> pure $ ContentText $ "Search results for '" <> q <> "' in " <> cat <> ": Found 8 products"
handleTool _ (AddToCart sku) = pure $ ContentText $ "Added item " <> sku <> " to your cart. Cart total: 3 items"
handleTool _ Checkout = pure $ ContentText "Checkout completed! Order #12345 confirmed. Thank you for shopping with us!"
handleTool _ (ComplexTool field1 field2 field3 field4 field5) =
    pure $ ContentText $ "Complex tool called with: " <> field1 <> ", " <> field2 <>
                        maybe "" (", " <>) field3 <> ", " <> field4 <>
                        maybe "" (", " <>) field5

main :: IO ()
main = do
    -- Set UTF-8 encoding to handle Unicode characters properly
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    -- Derive the handlers using Template Haskell
    let prompts = $(derivePromptHandler ''MyPrompt 'handlePrompt)
        resources = $(deriveResourceHandler ''MyResource 'handleResource)
        tools = $(deriveToolHandler ''MyTool 'handleTool)
     in runMcpServerStdio
        McpServerInfo
            { serverName = "Complete Example MCP Server"
            , serverVersion = "0.3.0"
            , serverInstructions = "An example MCP server that handles prompts, resources, and tools."
            }
        McpServerHandlers
            { prompts = Just prompts
            , resources = Just resources
            , tools = Just tools
            }
