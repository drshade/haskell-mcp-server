# mcp-server

[![Hackage](https://img.shields.io/hackage/v/mcp-server.svg)](https://hackage.haskell.org/package/mcp-server)
[![CI](https://github.com/drshade/haskell-mcp-server/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/drshade/haskell-mcp-server/actions/workflows/haskell-ci.yml)

Build [Model Context Protocol](https://modelcontextprotocol.io/) servers in
Haskell from plain data types. Declare your tools, prompts and resources as
ADTs, write one handler per type, and the library derives the JSON schemas,
argument decoding, validation and wire protocol for you — then serves it over
stdio or Streamable HTTP to Claude Code, Codex, Claude Desktop, Cursor and any
other MCP client.

```haskell
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Data.Text (Text)
import MCP.Server
import MCP.Server.Derive

data Units = Celsius | Fahrenheit

data WeatherTool
    = CurrentWeather { city :: Text, units :: Maybe Units }
    | Forecast       { city :: Text, days :: Int }

handleTool :: ClientContext -> WeatherTool -> IO Content
handleTool _ (CurrentWeather c _) = pure $ ContentText $ "Sunny in " <> c
handleTool _ (Forecast c n)       = pure $ ContentText $ "Forecast for " <> c

$(pure [])  -- end the declaration group so the splice below can see the types

main :: IO ()
main = runMcpServerStdio serverInfo noHandlers
    { tools = Just $(deriveToolHandler ''WeatherTool 'handleTool) }
  where
    serverInfo = McpServerInfo
      { serverName = "weather", serverVersion = "1.0.0"
      , serverInstructions = "Weather lookups" }
```

That is a complete, working MCP server exposing two tools, `current_weather`
and `forecast`, each with a JSON schema derived from its constructor's fields.

## What you get for free

The derivation reads your types, so the schema on the wire always matches the
handler that receives the arguments. Given this constructor from
[`examples/Complete`](examples/Complete/Types.hs):

```haskell
data ShippingSpeed = Standard | Express | Overnight

data Address = Address
    { street  :: Text
    , city    :: Text
    , zipCode :: Maybe Text
    }

data MyTool
    = Checkout { speed :: ShippingSpeed, shipTo :: Address }
    | ...
```

`tools/list` returns exactly this (captured from the running example):

```json
{
  "name": "checkout",
  "description": "Checkout",
  "annotations": { "destructiveHint": true },
  "inputSchema": {
    "type": "object",
    "required": ["speed", "shipTo"],
    "properties": {
      "speed":  { "type": "string", "enum": ["standard", "express", "overnight"] },
      "shipTo": {
        "type": "object",
        "required": ["street", "city"],
        "properties": {
          "street":  { "type": "string" },
          "city":    { "type": "string" },
          "zipCode": { "type": "string" }
        }
      }
    }
  }
}
```

and a `tools/call` with matching arguments arrives in your handler as a fully
decoded `Checkout Express (Address "1 Main St" "Springfield" Nothing)`.
Malformed arguments never reach you; the library answers with the appropriate
JSON-RPC error. The same machinery works in reverse for typed results (see
[Structured output](#structured-output)).

Beyond the derivation, the library handles:

- **Both protocol eras.** Legacy revisions `2024-11-05` through `2025-11-25`
  negotiated via `initialize`, and the stateless `2026-07-28` revision
  declared per request in `_meta`, from one server binary.
- **Two transports.** stdio, and Streamable HTTP with bearer auth, Origin
  validation, per-request SSE and a plain WAI application you can embed.
- **Long-running tools.** Progress notifications, per-request client logging,
  and cancellation of in-flight requests on both transports.
- **Live servers.** `listChanged` and resource-update pushes over
  `subscriptions/listen`.
- **Conformance fixtures.** A language-agnostic corpus of request/response
  pairs the test suite replays against every protocol era.

## Installation

Add `mcp-server` to your `build-depends`:

```cabal
build-depends:
  base, text, mcp-server
```

Tested against GHC 9.6 through 9.14 in CI. The HTTP transport requires
`ghc-options: -threaded` (Warp needs the threaded runtime).

## Connecting to a client

Build your server, then register it with your client of choice. The examples
below use the `simple-example` executable from this repository; substitute
your own.

### Claude Code

```bash
# stdio: everything after -- is the command Claude Code spawns
claude mcp add my-server -- "$(cabal list-bin exe:simple-example)"

# pass environment variables with --env
claude mcp add my-server --env API_KEY=secret -- /path/to/my-server

# Streamable HTTP
claude mcp add --transport http my-server http://localhost:3000/mcp
```

Verify with `claude mcp list` or `/mcp` inside a session. Add
`--scope project` to write a `.mcp.json` you can commit for your team:

```json
{
  "mcpServers": {
    "my-server": {
      "type": "stdio",
      "command": "/path/to/my-server",
      "env": { "API_KEY": "${API_KEY}" }
    }
  }
}
```

Cursor and several other clients read the same `mcpServers` shape.

### Codex

```bash
codex mcp add my-server --env API_KEY=secret -- /path/to/my-server
```

Or in `~/.codex/config.toml`, which is also where HTTP servers go:

```toml
[mcp_servers.my-server]
command = "/path/to/my-server"

[mcp_servers.my-http-server]
url = "http://localhost:3000/mcp"
```

### Claude Desktop

Claude Desktop launches stdio servers from `claude_desktop_config.json`. A
Docker image is a convenient way to ship a Haskell binary to it — the
repository's [`Dockerfile`](Dockerfile) builds all three examples:

```bash
docker build -t haskell-mcp-server .
```

```json
{
  "mcpServers": {
    "simple-example": {
      "command": "docker",
      "args": ["run", "-i", "--entrypoint=/usr/local/bin/simple-example", "haskell-mcp-server"]
    }
  }
}
```

### Keep stdout clean

On stdio, `stdout` carries only JSON-RPC. The library writes nothing else
there, and your handlers must not either: log to `stderr`.

## Defining tools

### Naming and arguments

Constructor names become snake_case tool names; record fields become named
arguments. The generated `inputSchema` mirrors the field types:

```haskell
data Color = Red | Green | Blue          -- all-nullary type: string enum
data Filters = Filters                   -- record: nested JSON object
  { tags     :: [Text]                   -- list: JSON array
  , maxCount :: Maybe Int                -- Maybe: optional field
  }

data MyTool
    = SearchItems                        -- "search_items"
      { query   :: Text
      , color   :: Color                 -- "red" | "green" | "blue"
      , filters :: Filters               -- { "tags": [...], "maxCount": ... }
      , limit   :: Maybe Int
      }
```

Primitive fields (`Int`, `Integer`, `Double`, `Float`, `Bool`, `Text`) are
parsed leniently: `42` and `"42"` are both accepted, since many clients send
numbers and booleans as strings.

A constructor may also wrap a single record type, which is unwrapped
recursively until a record is found:

```haskell
data SetValueParams = SetValueParams { key :: Text, value :: Text }

data SimpleTool
    = GetValue { key :: Text }
    | SetValue SetValueParams              -- fields of SetValueParams are the arguments
```

Positional (unnamed) fields are not supported, because they have no names to
put in the schema:

```haskell
data SimpleTool = GetValue Int | SetValue Int Text   -- ❌ rejected
```

### Results and errors

Simple handlers return `Content` (or `Text`). Return a `ToolResult` for
multiple content blocks or to report an execution failure with `isError`,
which the spec prefers over a protocol error so the model can see what went
wrong and react:

```haskell
handleTool :: ClientContext -> MyTool -> IO ToolResult
handleTool _ (SearchItems q _ _ _)
  | T.null q  = pure $ toolError "query must not be empty"
  | otherwise = pure $ toolResult [ContentText ("Results for " <> q)]
```

Content blocks can carry annotations (`audience`, `priority`,
`lastModified`) via the `ContentAnnotated` wrapper:

```haskell
ContentAnnotated defaultAnnotations { annotationsPriority = Just 0.9 }
                 (ContentText "important result")
```

### Structured output

Give the derivation a result type and it derives the tool's `outputSchema`
(same field rules as inputs) and serializes your value into
`structuredContent`, guaranteed to match. Per the spec's recommendation the
JSON is also returned as a text block for clients that predate structured
output:

```haskell
data WeatherReport = WeatherReport
    { temperature :: Int
    , sky         :: Sky          -- enum
    , alerts      :: [Text]
    , humidity    :: Maybe Int    -- omitted when Nothing
    }

handleTool :: ClientContext -> MyTool -> IO (ToolOutput WeatherReport)
handleTool _ (GetWeather city) = pure $ ToolOutput (lookupWeather city)
handleTool _ (BrokenTool _)    = pure $ ToolOutputError "sensor offline"

tools = Just $(deriveToolHandlerWithOutput ''MyTool 'handleTool ''WeatherReport)
```

`ToolOutputWith` supplies custom content blocks alongside the structured
value; `ToolOutputRaw` is the escape hatch back to a plain `ToolResult`.

### Descriptions, annotations and icons

Every `derive*` function has a `WithDescription` variant taking a flat list of
constructor and field descriptions, and a `WithOptions` variant taking
per-constructor `DefinitionOptions`: description, title, icons, behavioral
annotations (which drive client permission UX, such as auto-approving
read-only tools) and argument descriptions scoped to that constructor.

```haskell
descriptions =
  [ ("SearchItems", "Search the catalog")     -- constructor
  , ("query",       "Search terms")           -- field
  ]
tools = Just $(deriveToolHandlerWithDescription ''MyTool 'handleTool descriptions)
```

```haskell
tools = Just $(deriveToolHandlerWithOptions ''MyTool 'handleTool
  [ ("SearchItems", defaultDefinitionOptions
      { optDescription = Just "Search the catalog"
      , optToolAnnotations = Just defaultToolAnnotations
          { toolReadOnlyHint = Just True, toolIdempotentHint = Just True }
      , optIcons = [icon "https://example.com/search.png"]
      , optFieldDescriptions = [("query", "Search terms")]
      })
  ])
```

## Defining prompts

Prompts derive the same way. Arguments are string-valued per the spec, so
prompt records are limited to primitive and enumeration fields:

```haskell
data MyPrompt = Recipe { idea :: Text } | Shopping { items :: Text }

handlePrompt :: ClientContext -> MyPrompt -> IO Content
handlePrompt _ (Recipe idea)    = pure $ ContentText $ "Recipe for " <> idea
handlePrompt _ (Shopping items) = pure $ ContentText $ "Shopping list: " <> items

prompts = Just $(derivePromptHandler ''MyPrompt 'handlePrompt)
```

Return a `PromptResult` instead of `Content` for a description and a
multi-message conversation with user and assistant roles.

## Defining resources

Nullary constructors become static resources with `resource://` URIs; record
constructors become resource *templates* (RFC 6570), one percent-decoded path
segment per field:

```haskell
data MyResource
    = Menu                                            -- resource://menu
    | ProductDetail { sku :: Text }                   -- resource://product_detail/{sku}
    | OrderItem { orderId :: Int, itemName :: Text }  -- resource://order_item/{orderId}/{itemName}

handleResource :: ClientContext -> URI -> MyResource -> IO ResourceContent
handleResource _ uri Menu                = pure $ ResourceText uri "text/plain" "Today's menu..."
handleResource _ uri (ProductDetail sku) = pure $ ResourceText uri "text/plain" ("Details for " <> sku)
handleResource _ uri (OrderItem o i)     = ...

resources         = Just $(deriveResourceHandler ''MyResource 'handleResource)
resourceTemplates = Just $(deriveResourceTemplates ''MyResource)
```

The read handler matches template URIs such as
`resource://product_detail/ABC123` and decodes the segments into the
constructor's fields; typed fields like `Int` are parsed, and a failing
segment yields an invalid-params error.

## Argument completion

Provide a `completions` handler to serve `completion/complete` for prompt
arguments and resource-template parameters. The capability is advertised
automatically when the handler is present:

```haskell
handleComplete :: ClientContext -> CompletionRef -> ArgumentName -> Text -> Map Text Text
               -> IO (Either Error CompletionResult)
handleComplete _ (CompletionRefPrompt "recipe") "idea" partial _ =
    pure $ Right $ completionResult $
        filter (T.isPrefixOf partial) ["pancakes", "pasta", "pizza"]
handleComplete _ _ _ _ _ = pure $ Right $ completionResult []

handlers = noHandlers { completions = Just handleComplete, ... }
```

## Assembling the server

Start from `noHandlers` and record-update the features you provide.
Constructing `McpServerHandlers` directly is discouraged: the library grows
new handler slots over time, and a missed field fails at runtime rather than
compile time.

```haskell
handlers = noHandlers
  { prompts           = Just $(derivePromptHandler ''MyPrompt 'handlePrompt)
  , resources         = Just $(deriveResourceHandler ''MyResource 'handleResource)
  , resourceTemplates = Just $(deriveResourceTemplates ''MyResource)
  , tools             = Just $(deriveToolHandler ''MyTool 'handleTool)
  , completions       = Just handleComplete
  }
```

Two Template Haskell details to know:

- A `derive*` splice can only see types declared in an earlier declaration
  group. Either put the types in their own module (as the
  [examples](examples/) do) or end the group with an empty `$(pure [])`
  splice before the `main` that uses them.
- Every handler receives the per-request `ClientContext` first. It carries the
  caller's bearer token and principal on HTTP, the protocol revision and
  client identity for modern clients, and the `reportProgress` and
  `logToClient` actions described below.

### Manual handlers

The derived handlers are ordinary values, so for full control you can supply
your own instead. Prompt arguments arrive as `Map Text Text`, tool arguments
as `Map Text Value`:

```haskell
promptListHandler :: ClientContext -> IO [PromptDefinition]
promptGetHandler  :: ClientContext -> PromptName -> Map Text Text -> IO (Either Error PromptResult)

handlers = noHandlers { prompts = Just (promptListHandler, promptGetHandler) }
```

## Transports

### stdio

`runMcpServerStdio serverInfo handlers` serves JSON-RPC over stdin and
stdout. `runMcpServerStdioWithConfig` takes a `StdioConfig` for verbose
request logging on stderr, cacheability hints for modern clients, and a
change-notification source.

### Streamable HTTP

```haskell
import MCP.Server.Transport.Http

main = runMcpServerHttp serverInfo handlers            -- localhost:3000/mcp

main = runMcpServerHttpWithConfig defaultHttpConfig
    { httpPort = 8080
    , httpHost = "0.0.0.0"
    , httpEndpoint = "/api/mcp"
    , httpVerbose = True                                -- request/response logging on stderr
    , httpAllowedOrigins = Just ["https://app.example.com"]
    } serverInfo handlers
```

`httpAllowedOrigins` is DNS-rebinding protection: requests carrying an
`Origin` outside the list get 403. `Nothing` disables the check and is only
appropriate for servers unreachable from browsers.

**Bearer-token authentication** is a callback. Return `Just principal` (any
JSON `Value`, such as a role) to admit the request, or `Nothing` for 401. The
principal reaches handlers as `clientPrincipal` in the `ClientContext`; token
policy lives entirely in your application.

```haskell
defaultHttpConfig
  { httpAuthorize = Just $ \mtoken -> case mtoken of
      Just "secret-admin-token" -> pure $ Just (String "admin")
      Just "secret-user-token"  -> pure $ Just (String "user")
      _                         -> pure Nothing
  }
```

The endpoint accepts POST only. Server-to-client notifications flow over the
`subscriptions/listen` POST response stream rather than the deprecated
standalone GET stream, and CORS is enabled for web clients.

### Embedding in an existing WAI stack

The MCP endpoint is a plain [WAI](https://hackage.haskell.org/package/wai)
application, exported as `mcpApplication`, so it can be mounted inside your
own Warp settings, TLS, middleware or router:

```haskell
import MCP.Server (mcpApplication, defaultHttpConfig)
import qualified Network.Wai.Handler.Warp as Warp

main = Warp.runSettings mySettings $ \req respond ->
    -- route /mcp to the MCP endpoint, everything else to your app
    mcpApplication defaultHttpConfig serverInfo handlers req respond
```

`httpPort` and `httpHost` are ignored when embedding; the endpoint path,
Origin validation, bearer auth and streaming all apply as usual.

## Long-running tools

### Progress and logging

Handlers report progress and send log messages to the calling client through
actions on the `ClientContext`. Both are safe to call unconditionally:

```haskell
handleTool ctx (ImportData file) = do
    reportProgress ctx 0.0 (Just 1.0) (Just "starting import")
    logToClient ctx LogInfo (String "opening file")
    ...
    reportProgress ctx 1.0 (Just 1.0) Nothing
```

- `reportProgress` emits `notifications/progress` only when the request
  carried a `progressToken`. Progress values must increase call over call.
- `logToClient` emits `notifications/message` only when the request declared
  `io.modelcontextprotocol/logLevel`, as the spec requires, and drops
  messages below the declared level.

On stdio the notifications interleave before the response. On HTTP, a request
that opted in is answered with an SSE stream carrying the notifications
followed by the final response; other requests keep the single-JSON response.

### Cancellation

In-flight requests can be cancelled, after which the server stops work as soon
as practical and sends nothing further for that request:

- **stdio**: each request runs in its own task, and a `notifications/cancelled`
  naming its id cancels that task. Unknown or completed ids are ignored.
- **HTTP**: closing the response stream is the cancellation signal. SSE
  responses detect the disconnect within one keep-alive interval. Single-JSON
  responses only detect it at the final write, so clients wanting cancellable
  calls should opt into streaming via a `progressToken`.

Cancellation is delivered as an asynchronous exception, the standard GHC
mechanism used by `timeout` and `cancel`. Handlers are interruptible wherever
they block in `IO`, and one that acquires resources should release them with
`bracket` or `finally`:

```haskell
handleTool ctx (ImportData file) =
    bracket (openFile file ReadMode) hClose $ \h -> do
        ...
```

Critical sections can be shielded with `mask`, but keep them short: cancellation
waits for them.

### Concurrency

Requests are served concurrently on both transports. Handlers touching shared
mutable state must synchronize with `MVar`, `STM` or similar.

## Change notifications

Servers whose tool, prompt or resource lists change at runtime can push
change notifications. Create a notifier, hand its source to the transport, and
call the notifier when things change:

```haskell
main = do
    (notifier, source) <- newMcpNotifier
    _ <- forkIO $ appLogic notifier   -- calls notifyToolsListChanged etc.
    runMcpServerStdioWithConfig
        defaultStdioConfig { stdioNotifications = Just source }
        serverInfo handlers
```

Delivery is transport- and era-aware, and the `listChanged` and `subscribe`
capabilities are advertised only where delivery is possible:

- **Modern clients (2026-07-28)** open a `subscriptions/listen` stream (a
  long-lived SSE response over HTTP) and receive only the notification types
  they opted into, tagged with their subscription id, including
  `notifications/resources/updated` for watched URIs.
- **Legacy stdio clients** receive spontaneous untagged notifications once
  their `notifications/initialized` arrives.
- **Legacy HTTP clients** have no delivery channel, so nothing is advertised.

## Protocol support

| Feature | Legacy (`2024-11-05` to `2025-11-25`) | Modern (`2026-07-28`) |
| --- | --- | --- |
| Version selection | `initialize` handshake | per-request `_meta`, `server/discover` |
| Prompts, resources, resource templates, tools | ✅ | ✅ |
| Argument completion | ✅ | ✅ |
| Tool annotations, icons, structured output | ✅ | ✅ |
| Progress and per-request logging | ✅ | ✅ |
| Cancellation | ✅ | ✅ |
| Change notifications | stdio only | `subscriptions/listen` (stdio and HTTP) |
| Result typing and cacheability hints | — | `resultType`, `httpCacheHints` |
| HTTP request-metadata headers | — | `MCP-Protocol-Version`, `Mcp-Method`, `Mcp-Name` validated |

Design decisions and planned work (input-required results, OAuth resource
metadata, pagination, the tasks extension) live as ADRs under
[`specs/`](specs/README.md), ordered by [`specs/ROADMAP.md`](specs/ROADMAP.md).

## Examples

- [`examples/Simple/`](examples/Simple/): a key-value store with two tools over stdio.
- [`examples/Complete/`](examples/Complete/): prompts, resources, a resource
  template, tools with enum, nested and list arguments, `isError`,
  annotations, progress and completions.
- [`examples/HttpSimple/`](examples/HttpSimple/): the key-value store over
  Streamable HTTP.

```bash
cabal run simple-example        # stdio; type JSON-RPC on stdin
cabal run http-simple-example   # http://localhost:3000/mcp
```

## Conformance corpus

The wire-format fixtures under [`test/golden/`](test/golden/README.md) are a
language-agnostic MCP conformance corpus: each case is a raw JSON-RPC
`.request.json` and the exact `.response.json` a reference server answers,
per protocol era, enumerated by a `manifest.json`. Any MCP implementation
that reproduces the small reference server described there can replay the
requests and diff the responses. Contributions of new cases are welcome.

## Documentation

- [API documentation on Hackage](https://hackage.haskell.org/package/mcp-server)
- [MCP Specification (2026-07-28)](https://modelcontextprotocol.io/specification/2026-07-28/)
- [MCP Specification (2025-11-25, newest legacy revision)](https://modelcontextprotocol.io/specification/2025-11-25/)

## Contributing

Contributions are welcome. See the issue tracker for open issues and feature
requests.

## AI assistance

Much of this library was written with Claude, working from a specification I
wrote and iterating together until I was happy with the result. I review and
maintain all of it, but parts such as the Template Haskell derivation sit
outside what I would have written unaided, and I expect to keep refactoring
them.

## License

BSD-3-Clause
