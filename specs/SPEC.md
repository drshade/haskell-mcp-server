
# NOTICE

This file is kept for brevity - but served as my starting specification before writing the library. This may no longer be useful and will be superceded by examples in the examples/ folder.

# Haskell MCP Server Library

Fully featured haskell library for building MCP Servers.

## MCP Specification - Model Context Protocol

The MCP spec is a specialisation of the JSON-RPC specification, exposing a number of requests and responses. This library allows developers to build these servers easily in haskell.

This library is compliant with the following MCP specifications for building MCP Servers which can be found here:

| Item | Spec URL |
| Overview | https://modelcontextprotocol.io/specification/2025-06-18/server |
| Prompts | https://modelcontextprotocol.io/specification/2025-06-18/server/prompts |
| Resources | https://modelcontextprotocol.io/specification/2025-06-18/server/resources |
| Tools | https://modelcontextprotocol.io/specification/2025-06-18/server/tools |

# Library itself

Supports 2 levels of granularity:

## 1. Lowest-level fine grained handling of Prompts, Resources and Tools, e.g.:

Using an example of a grocery shopping MCP server:

```haskell

import MCP.Server.Types (Content(ContentText, ContentImage), Error(InvalidPromptName, MissingRequiredParams, ResourceNotFound, InternalError, UnknownTool), PromptDefinition, ResourceDefinition, ArgumentDefinition, InputSchemaDefinition(..), PaginatedResult(..), Cursor)
import Network.URI (URI(URI))
import qualified Data.Map as Map

type PromptName = Text
type ToolName = Text
type ArgumentName = Text
type ArgumentValue = Text

handleListPrompts :: (Monad m) => Maybe Cursor -> m (PaginatedResult [PromptDefinition])
handleListPrompts cursor =
    pure $ PaginatedResult
        { paginatedItems = 
            [ PromptDefinition
                { promptDefinitionName = "recipe"
                , promptDefinitionDescription = "Generate a detailed recipe for a particular idea" 
                , promptDefinitionArguments = 
                    [ ArgumentDefinition
                        { argumentDefinitionName = "idea"
                        , argumentDefinitionDescription = "inspiring idea for the recipe" 
                        , argumentDefinitionRequired = True
                        }
                    ]
                }
            ]
        , paginatedNextCursor = Nothing  -- No more pages
        }

handleListResources :: (Monad m) => Maybe Cursor -> m (PaginatedResult [ResourceDefinition])
handleListResources cursor = 
    pure $ PaginatedResult
        { paginatedItems =
            [ ResourceDefinition
                { resourceDefinitionURI = "file:///product_categories"
                , resourceDefinitionName = "product_categories"
                , resourceDefinitionDescription = "List of product categories"
                , resourceDefinitionMimeType = "text/plain"
                }
            ]
        , paginatedNextCursor = Nothing  -- No more pages
        }

handleListTools :: (Monad m) => Maybe Cursor -> m (PaginatedResult [ToolDefinition])
handleListTools cursor = 
    pure $ PaginatedResult
        { paginatedItems =
            [ ToolDefinition
                { toolDefinitionName = "search_for_product" 
                , toolDefinitionDescription = "Search for products in the catalog"
                , toolDefinitionInputSchema = 
                    InputSchemaDefinitionObject
                        { properties = 
                            [   ("q"
                                , InputSchemaDefinitionProperty
                                    { type = "string"
                                    , description = "Matching this query, using 'contains' semantics"
                                    }
                                )
                            ,   ("category"
                                , InputSchemaDefinitionProperty
                                    { type = "string"
                                    , description = "Limit to searching within this category (optional)"
                                    }
                                )
                            ]
                        , required = ["q"]
                        }
                }
            ]
        , paginatedNextCursor = Nothing  -- No more pages
        }

handleGetPrompt :: (Monad m) => PromptName -> [(ArgumentName, ArgumentValue)] -> m (Either Error Content)
handleGetPrompt "recipe" args = do
    let idea = Map.lookup "idea" $ Map.fromList args  -- idea is required
     in case idea of
            Just idea' -> pure $ Right $ ContentText $ "Recipe prompt for " <> idea' <> " ..."
            Nothing -> pure $ Left $ MissingRequiredParams "field 'idea' is missing"
handleGetPrompt prompt _ = pure $ Left $ InvalidPromptName $ prompt <> " unknown!"

handleReadResource :: (Monad m) => URI -> m (Either Error Content)
handleReadResource uri = do
    case uriPath uri of
        "/product_categories" -> pure $ Right $ ContentText $ "category 1, category 2"
        unknown -> pure $ Left $ ResourceNotFound $ "Resouce " <> unknown <> " not found!"

handleCallTool :: (Monad m) => ToolName -> [(ArgumentName, ArgumentValue)] -> m (Either Error Content)
handleCallTool "search_for_product" args = do
    let q = Map.lookup "q" $ Map.fromList args                  -- q is required
    let category = Map.lookup "category" $ Map.fromList args    -- category is optional
     in case q of
            Nothing -> pure $ Left $ MissingRequiredParams "field 'q' is missing"
            Just q' -> pure $ Right $ ContentText $ "product 1, product 2"
handleCallTool tool _ = pure $ Left $ UnknownTool $ tool <> " unknown!"

-- Running the MCP server using STDIO

main :: IO ()
main =
    runMcpServerStdio
        McpServerInfo
            { serverName = "Products available at your Grocery Store"
            , serverVersion = "0.1.0"
            , serverInstructions = "Use these tools to fetch information about products available at your grocery store, including prices, ingredients, descriptions etc."
            }
        McpServerHandlers
            { prompts = Just (handleListPrompts, handleGetPrompt)
            , resources = Just (handleListResources, handleReadResource)
            , tools = Just (handleListTools, handleCallTool)
            }

```

## 2. High-level derived interfaces for simpler definitions

High level MCP server definitions take advantage of Template Haskell for generating all the boilerplate of the low-level definitions, but allow for much simpler and type safe definitions such as in the example below.

```haskell

import MCP.Server.Types (Content(ContentText, ContentImage))

data MyPrompt
    = Recipe { idea :: Text }
    | Shopping { description :: Text }

data MyResource
    = ProductCategories
    | SaleItems
    | HeadlineBannerAd

data MyTool
    = SearchForProduct { q :: Text, category :: Maybe Text }
    | AddToCart { sku :: Text }
    | Checkout

handlePrompt :: (MonadIO m) => MyPrompt -> m Content
handlePrompt (Recipe idea) =
    pure $ ContentText "Recipe prompt goes here..."
handlePrompt (Shopping description) =
    pure $ ContentText "Shopping prompt goes here..."

handleResource :: (MonadIO m) => MyResource -> m Content
handleResource ProductCategories = pure $ ContentText "category 1, category 2"
handleResource SaleItems = pure $ ContentText "item 1, item 2, item 3"
handleResource HeadlineBannerAd = pure $ ContentImage { mimeType = "image/png", data = "..." }

handleTool :: (MonadIO m) => MyTool -> m Content
handleTool (SearchForProduct q category) = pure $ ContentText "search results..."
handleTool (AddToCart sku) = pure $ ContentText "Item added to cart!"
handleTool Checkout = pure $ ContentText "checkout completed!"

main =
    -- Derive the handlers
    let prompts = $(derivePromptHandler ''MyPrompt 'handlePrompt)
        resources = $(deriveResourceHandler ''MyResource 'handleResource)
        tools = $(deriveToolHandler ''MyTool 'handleTool)
     in runMcpServerStdio
        McpServerInfo
            { serverName = "Products available at your Grocery Store"
            , serverVersion = "0.1.0"
            , serverInstructions = "Use these tools to fetch information about products available at your grocery store, including prices, ingredients, descriptions etc."
            }
        McpServerHandlers
            { prompts = Just prompts
            , resources = Just resources
            , tools = Just tools
            }

```

