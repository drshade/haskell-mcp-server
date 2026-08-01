# mcp-server

A fully-featured Haskell library for building [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) servers.

## Features

- **Complete MCP Implementation**: Dual-era server — legacy revisions `2024-11-05` through `2025-11-25` via the `initialize` handshake, and the stateless `2026-07-28` revision via per-request `_meta` (including `server/discover`, `resultType`, and cacheability fields)
- **Type-Safe API**: Leverage Haskell's type system for robust MCP servers
- **Multiple Abstractions**: Both low-level fine-grained control and high-level derived interfaces
- **Template Haskell Support**: Automatic handler derivation from data types
- **Multiple Transports**: STDIO and HTTP Streaming transport (MCP Streamable HTTP)

## Supported MCP Features

- ✅ **Prompts**: User-controlled prompt templates with arguments
- ✅ **Resources**: Application-controlled readable resources
- ✅ **Resource Templates**: Parameterized resources via URI templates
- ✅ **Tools**: Model-controlled callable functions
- ✅ **Completions**: Argument autocompletion for prompts and templates
- ✅ **Change Notifications**: `listChanged`/resource-update pushes, via `subscriptions/listen` (2026-07-28) or legacy stdio delivery
- ✅ **Initialization Flow**: Complete protocol lifecycle with version negotiation
- ✅ **Error Handling**: Comprehensive error types and JSON-RPC error responses

## Quick Start

Add the library `mcp-server` to your cabal file:

```cabal
build-depends:
  mcp-server
```

Create a simple module, such as this example below:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Text (Text)
import MCP.Server
import MCP.Server.Derive

-- Define your data types
data MyPrompt = Recipe { idea :: Text } | Shopping { items :: Text }
data MyResource = Menu | Specials
data MyTool = Search { query :: Text } | Order { item :: Text }

-- Implement handlers. Every handler receives the per-request 'ClientContext'
-- (the caller's bearer token and principal on the HTTP transport) first.
handlePrompt :: ClientContext -> MyPrompt -> IO Content
handlePrompt _ (Recipe idea) = pure $ ContentText $ "Recipe for " <> idea
handlePrompt _ (Shopping items) = pure $ ContentText $ "Shopping list: " <> items

handleResource :: ClientContext -> URI -> MyResource -> IO ResourceContent
handleResource _ uri Menu = pure $ ResourceText uri "text/plain" "Today's menu..."
handleResource _ uri Specials = pure $ ResourceText uri "text/plain" "Daily specials..."

handleTool :: ClientContext -> MyTool -> IO Content
handleTool _ (Search query) = pure $ ContentText $ "Search results for " <> query
handleTool _ (Order item) = pure $ ContentText $ "Ordered " <> item

-- Template Haskell staging: this empty splice ends the declaration group,
-- so the derive splices below can see the types above. (Alternatively,
-- declare the types in a separate module, as the examples/ do.)
$(pure [])

-- Derive handlers automatically
main :: IO ()
main = runMcpServerStdio serverInfo handlers
  where
    serverInfo = McpServerInfo
      { serverName = "My MCP Server"
      , serverVersion = "1.0.0"
      , serverInstructions = "A sample MCP server"
      }
    -- Start from 'noHandlers' and record-update the features you provide:
    -- constructing McpServerHandlers directly breaks (at runtime!) when a
    -- field is missed, and the library grows new handler slots over time.
    handlers = noHandlers
      { prompts = Just $(derivePromptHandler ''MyPrompt 'handlePrompt)
      , resources = Just $(deriveResourceHandler ''MyResource 'handleResource)
      , tools = Just $(deriveToolHandler ''MyTool 'handleTool)
      }
```

### Advanced Template Haskell Features

#### Automatic Naming Conventions

Constructor names are automatically converted to snake_case for MCP names:

```haskell
data MyTool = GetValue | SetValue | SearchItems
-- Becomes: "get_value", "set_value", "search_items"
```

#### Typed Tool Arguments

Tool arguments are decoded from full JSON values, and the generated
`inputSchema` mirrors the field types:

```haskell
data Color = Red | Green | Blue          -- all-nullary type: string enum
data Filters = Filters                   -- record: nested JSON object
  { tags     :: [Text]                   -- list: JSON array
  , maxCount :: Maybe Int                -- Maybe: optional field
  }

data MyTool = Search
  { query   :: Text
  , color   :: Color                     -- "red" | "green" | "blue"
  , filters :: Filters                   -- { "tags": [...], "maxCount": ... }
  , limit   :: Maybe Int
  }
```

Primitive fields (`Int`, `Integer`, `Double`, `Float`, `Bool`, `Text`) are
parsed leniently: the native JSON type and its string representation are
both accepted (`42` or `"42"`), since many clients send numbers and
booleans as strings.

Prompt arguments are string-valued per the MCP specification, so prompt
records are limited to primitive and enumeration fields.

#### Tool Results

Simple handlers can return plain `Content` (or `Text`). Return a full
`ToolResult` for multiple content blocks, structured content, or to report
execution failures with `isError` — which the spec prefers over protocol
errors, so the model can see what went wrong and react:

```haskell
handleTool :: ClientContext -> MyTool -> IO ToolResult
handleTool _ (Search q _ _ _)
  | T.null q  = pure $ toolError "query must not be empty"
  | otherwise = pure $ toolResult [ContentText ("Results for " <> q)]
```

Prompt handlers can likewise return a `PromptResult` with a description and
a multi-message conversation (user and assistant roles).

#### Nested Parameter Types

You can nest parameter types with automatic unwrapping:

```haskell
-- Parameter record types
data GetValueParams = GetValueParams { _gvpKey :: Text }
data SetValueParams = SetValueParams { _svpKey :: Text, _svpValue :: Text }

-- Main tool type
data SimpleTool
    = GetValue GetValueParams
    | SetValue SetValueParams
    deriving (Show, Eq)
```

The Template Haskell derivation recursively unwraps single-parameter constructors until it reaches a record type, then extracts all fields for the MCP schema.

#### Resource URI Generation

Resources automatically get `resource://` URIs based on constructor names:

```haskell
data MyResource = Menu | Specials
-- Generates: "resource://menu", "resource://specials"
```

#### Resource Templates

Record constructors become parameterized resource *templates* (RFC 6570 URI
templates), with one percent-decoded path segment per field:

```haskell
data MyResource
    = Menu                                            -- static: resource://menu
    | ProductDetail { sku :: Text }                   -- resource://product_detail/{sku}
    | OrderItem { orderId :: Int, itemName :: Text }  -- resource://order_item/{orderId}/{itemName}
```

The read handler derived by `deriveResourceHandler` matches template URIs
(e.g. `resource://product_detail/ABC123`) and decodes the segments into the
constructor's fields — typed fields like `Int` are parsed, and a failing
segment yields an invalid-params error. Advertise the templates via
`resources/templates/list` with:

```haskell
resourceTemplates = Just $(deriveResourceTemplates ''MyResource)
```

#### Argument Completion

Provide a `completions` handler to serve `completion/complete` for prompt
arguments and resource-template parameters:

```haskell
handleComplete :: ClientContext -> CompletionRef -> ArgumentName -> Text -> Map Text Text -> IO (Either Error CompletionResult)
handleComplete _ (CompletionRefPrompt "recipe") "idea" partial _ =
    pure $ Right $ completionResult $
        filter (T.isPrefixOf partial) ["pancakes", "pasta", "pizza"]
handleComplete _ _ _ _ _ = pure $ Right $ completionResult []
```

The `completions` capability is advertised automatically when the handler is
present.

#### Unsupported Patterns

We do not support positional (unnamed) parameters:

```haskell
-- ❌ This won't work - no field names
data SimpleTool
    = GetValue Int
    | SetValue Int Text
```

All parameter types must ultimately resolve to records with named fields to generate proper MCP schemas.

## Custom Descriptions

You can provide custom descriptions for constructors and fields using the `*WithDescription` variants:

```haskell
-- Define descriptions for constructors and fields
descriptions :: [(String, String)]
descriptions =
  [ ("Recipe", "Generate a recipe for a specific dish")     -- Constructor description
  , ("Search", "Search our menu database")                  -- Constructor description
  , ("idea", "The dish you want a recipe for")              -- Field description
  , ("query", "Search terms to find menu items")            -- Field description
  ]

-- Use in derivation
handlers = noHandlers
  { prompts = Just $(derivePromptHandlerWithDescription ''MyPrompt 'handlePrompt descriptions)
  , tools = Just $(deriveToolHandlerWithDescription ''MyTool 'handleTool descriptions)
  , resources = Just $(deriveResourceHandlerWithDescription ''MyResource 'handleResource descriptions)
  }
```

## Manual Handler Implementation

For fine-grained control, implement handlers manually:

```haskell
import MCP.Server

-- Manual handler implementation. Every handler receives the per-request
-- 'ClientContext' as its first argument. Prompt arguments are string-valued
-- (Map Text Text); tool arguments are full JSON values (Map Text Value).
promptListHandler :: ClientContext -> IO [PromptDefinition]
promptGetHandler :: ClientContext -> PromptName -> Map Text Text -> IO (Either Error PromptResult)
-- ... implement your custom logic

main :: IO ()
main = runMcpServerStdio serverInfo handlers
  where
    handlers = noHandlers
      { prompts = Just (promptListHandler, promptGetHandler)
      }
```

## Change Notifications

Servers whose tool/prompt/resource lists change at runtime can push change
notifications. Create a notifier, hand its source to the transport, and call
the notifier when things change:

```haskell
main :: IO ()
main = do
    (notifier, source) <- newMcpNotifier
    _ <- forkIO $ appLogic notifier   -- calls notifyToolsListChanged etc.
    runMcpServerStdioWithConfig
        defaultStdioConfig { stdioNotifications = Just source }
        serverInfo handlers
```

Delivery is transport- and era-aware, and the `listChanged`/`subscribe`
capabilities are advertised automatically where delivery is actually
possible:

- **Modern clients (2026-07-28)** open a `subscriptions/listen` stream (a
  long-lived SSE response over HTTP) and receive only the notification types
  they opted into, tagged with their subscription id — including
  `notifications/resources/updated` for watched URIs.
- **Legacy stdio clients** receive spontaneous untagged notifications once
  their `notifications/initialized` arrives (the lifecycle's ready signal).
- **Legacy HTTP clients** have no delivery channel (this library does not
  offer the deprecated GET SSE stream), so nothing is advertised to them.

## HTTP Transport

The library supports the MCP Streamable HTTP transport. Compile your
executable with `ghc-options: -threaded` — Warp requires the threaded runtime:

```haskell
import MCP.Server.Transport.Http

-- Simple HTTP server (localhost:3000/mcp)
main = runMcpServerHttp serverInfo handlers

-- Custom configuration
main = runMcpServerHttpWithConfig customConfig serverInfo handlers
  where
    customConfig = defaultHttpConfig
      { httpPort = 8080
      , httpHost = "0.0.0.0"
      , httpEndpoint = "/api/mcp"
      , httpVerbose = True     -- Enable detailed logging
      , httpAllowedOrigins = Just ["https://app.example.com"]
          -- Origin validation (DNS-rebinding protection): requests with an
          -- Origin header outside this list are rejected with 403. Nothing
          -- disables the check (only for servers unreachable from browsers).
      }
```

**Bearer-token authentication** (optional): supply an `httpAuthorize` callback
to validate the `Authorization: Bearer` token each request presents. Return
`Just principal` to authorize (the principal — any JSON `Value`, e.g. a role —
reaches your handlers as `clientPrincipal` in the `ClientContext`), or
`Nothing` to reject the request with 401. Token policy lives entirely in your
application; the library only threads the identity through:

```haskell
    customConfig = defaultHttpConfig
      { httpAuthorize = Just $ \mtoken -> case mtoken of
          Just "secret-admin-token" -> pure $ Just (String "admin")
          Just "secret-user-token"  -> pure $ Just (String "user")
          _                         -> pure Nothing
      }
```

**Features:**
- CORS enabled for web clients
- POST `/mcp` for JSON-RPC messages (GET returns 405 — server-to-client
  notifications flow over the `subscriptions/listen` POST response stream,
  not a standalone GET stream)
- Dual-era protocol support: legacy revisions (`2024-11-05`–`2025-11-25`)
  negotiate via `initialize`; the stateless `2026-07-28` revision declares its
  version per request in `_meta`, with full request-metadata header
  validation (`MCP-Protocol-Version`, `Mcp-Method`, `Mcp-Name` including the
  base64 sentinel encoding)
- Change notifications as long-lived SSE streams via `httpNotifications`
  (see [Change Notifications](#change-notifications))
- Optional pluggable bearer-token authentication via `httpAuthorize`
- Origin validation via `httpAllowedOrigins`
- Cacheability hints for modern list/read results via `httpCacheHints`

### Embedding in an existing WAI stack

`runMcpServerHttp` starts its own Warp server, but the MCP endpoint is a
plain [WAI](https://hackage.haskell.org/package/wai) application underneath,
and it is exported — so you can mount it inside whatever you already run
(your own Warp settings, TLS, middleware, or a larger router):

```haskell
import MCP.Server (mcpApplication, defaultHttpConfig)
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.runSettings mySettings $ \req respond ->
    -- route /mcp to the MCP endpoint, everything else to your app
    mcpApplication defaultHttpConfig serverInfo handlers req respond
```

`httpPort`/`httpHost` are ignored when embedding (they only configure the
server `runMcpServerHttp` starts); the endpoint path, Origin validation,
bearer auth and `subscriptions/listen` streaming all apply as usual.

## Conformance corpus

The wire-format fixtures under
[`test/golden/`](test/golden/README.md) double as an **API-agnostic MCP
conformance corpus**: each case is a raw JSON-RPC `.request.json` and the
exact `.response.json` a reference server answers, per protocol era
(legacy `initialize`-negotiated revisions and the stateless `2026-07-28`
revision), enumerated by a `manifest.json`. Nothing in the corpus is
Haskell-specific — any MCP server implementation that reproduces the small
reference server described in the corpus README can replay the requests and
diff the responses. Contributions of new cases are welcome.

## Roadmap

Design decisions and planned work live as ADRs under
[`specs/`](specs/README.md), ordered by [`specs/ROADMAP.md`](specs/ROADMAP.md).

## Examples

The library includes several examples:

- **`examples/Simple/`**: Basic key-value store using Template Haskell derivation (STDIO)
- **`examples/Complete/`**: Full-featured example with prompts, resources, a resource template, tools (enum/nested/list arguments, `isError`), and completions (STDIO)
- **`examples/HttpSimple/`**: HTTP version of the simple key-value store

## Docker Usage

I like to build and publish my MCP servers to Docker - which means that it's much easier to configure assistants such as Claude Desktop to run them.

```bash
# Build the image
docker build -t haskell-mcp-server .

# Run different examples
docker run -i --entrypoint="/usr/local/bin/simple-example" haskell-mcp-server
```

And then configure Claude by editing `claude_desktop_config.json`:

```json
{
    "mcpServers": {
       "simple-example": {
            "command": "docker",
            "args": [
                "run",
                "-i",
                "--entrypoint=/usr/local/bin/simple-example",
                "haskell-mcp-server"
            ]
        }
    }
}
```

## Documentation

- [MCP Specification (2026-07-28)](https://modelcontextprotocol.io/specification/2026-07-28/)
- [MCP Specification (2025-11-25, newest legacy revision)](https://modelcontextprotocol.io/specification/2025-11-25/)
- [API Documentation](https://hackage.haskell.org/package/mcp-server)
- [Examples](examples/)

## Contributing

Contributions are welcome! Please see the issue tracker for open issues and feature requests.

## Disclaimer - AI Assistance

I am not sure whether there is any stigma associated with this but Claude helped me write a lot of this library. I started with a very specific specification of what I wanted to achieve and worked shoulder-to-shoulder with Claude to implement and refactor the library until I was happy with it. A few of the features such as the Derive functions are a little out of my comfort zone to have manually written, so I appreciated having an expert guide me here - however I do suspect that this implementation may be sub-par and I do intend to refactor and rewrite large pieces of this through regular maintenance.

## License

BSD-3-Clause