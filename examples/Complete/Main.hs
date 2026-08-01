{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.Map  (Map)
import           Data.Text (Text)
import qualified Data.Text as T
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
handleResource _ uri (ProductDetail sku) =
    pure $ ResourceText uri "text/plain" $ "Details for product " <> sku <> ": in stock, $9.99"

-- Tool handlers return a full ToolResult: execution failures are reported
-- with isError so the model can see them (returning plain Content works too
-- for simple tools — see the other examples).
handleTool :: ClientContext -> MyTool -> IO ToolResult
handleTool ctx (SearchForProduct q category) = do
    -- Progress reporting is safe to call unconditionally: it is a no-op
    -- unless the request carried a progressToken
    reportProgress ctx 0.5 (Just 1.0) (Just "searching the catalog")
    result <- case category of
        Nothing -> pure $ toolResult [ContentText $ "Search results for '" <> q <> "': Found 15 products across all categories"]
        Just cat -> pure $ toolResult [ContentText $ "Search results for '" <> q <> "' in " <> cat <> ": Found 8 products"]
    reportProgress ctx 1.0 (Just 1.0) Nothing
    pure result
handleTool _ (AddToCart sku quantities)
    | any (<= 0) quantities = pure $ toolError "Quantities must be positive"
    | otherwise = pure $ toolResult
        [ContentText $ "Added " <> T.pack (show (sum quantities)) <> " of item " <> sku <> " to your cart"]
handleTool _ (Checkout speed (Address street city zipCode)) =
    pure $ toolResult
        [ ContentText $ "Checkout completed! Order #12345 will ship "
            <> T.pack (show speed) <> " to " <> street <> ", " <> city
            <> maybe "" (" " <>) zipCode
        ]
handleTool _ (ComplexTool field1 field2 field3 field4 field5) =
    pure $ toolResult
        [ContentText $ "Complex tool called with: " <> field1 <> ", " <> field2 <>
                        maybe "" (", " <>) field3 <> ", " <> field4 <>
                        maybe "" (", " <>) field5]

-- Argument autocompletion for the Recipe prompt's idea argument
handleComplete :: ClientContext -> CompletionRef -> ArgumentName -> Text -> Map Text Text -> IO (Either Error CompletionResult)
handleComplete _ (CompletionRefPrompt "recipe") "idea" partial _ =
    pure $ Right $ completionResult $
        filter (T.isPrefixOf (T.toLower partial)) ["pancakes", "pasta", "pizza"]
handleComplete _ _ _ _ _ = pure $ Right $ completionResult []

main :: IO ()
main = do
    -- Set UTF-8 encoding to handle Unicode characters properly
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    -- Derive the handlers using Template Haskell
    let prompts = $(derivePromptHandler ''MyPrompt 'handlePrompt)
        resources = $(deriveResourceHandler ''MyResource 'handleResource)
        templates = $(deriveResourceTemplates ''MyResource)
        -- Per-constructor options: descriptions, behavioral hints for
        -- client permission UX, icons, and constructor-scoped argument
        -- descriptions
        tools = $(deriveToolHandlerWithOptions ''MyTool 'handleTool
          [ ("SearchForProduct", defaultDefinitionOptions
              { optDescription = Just "Search the product catalog"
              , optToolAnnotations = Just defaultToolAnnotations
                  { toolReadOnlyHint = Just True, toolIdempotentHint = Just True }
              , optFieldDescriptions = [("q", "Search terms"), ("category", "Restrict to a category")]
              })
          , ("Checkout", defaultDefinitionOptions
              { optToolAnnotations = Just defaultToolAnnotations
                  { toolDestructiveHint = Just True }
              })
          ])
     in runMcpServerStdio
        McpServerInfo
            { serverName = "Complete Example MCP Server"
            , serverVersion = "0.3.0"
            , serverInstructions = "An example MCP server that handles prompts, resources, and tools."
            }
        noHandlers
            { prompts = Just prompts
            , resources = Just resources
            , resourceTemplates = Just templates
            , tools = Just tools
            , completions = Just handleComplete
            }
