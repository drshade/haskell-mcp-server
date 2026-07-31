# Revision history for mcp-server

## 0.2.0.0 - ???

A major overhaul of the handler API. The headline changes: the handler
boundary is no longer stringly typed, and the server is dual-era — it speaks
both the legacy initialize-handshake revisions and the stateless
`2026-07-28` revision.

### Dual-era protocol support (2026-07-28)

* Requests that declare a protocol revision in their params `_meta`
  (`io.modelcontextprotocol/protocolVersion`) are served statelessly with
  the modern result envelope: `resultType: "complete"`, the server identity
  in result `_meta`, and — on `tools/list`, `prompts/list`,
  `resources/list`, `resources/read` and `server/discover` — the required
  `ttlMs`/`cacheScope` fields (configurable via `CacheHints` on the
  transport configs; default: no caching, private). Requests without modern
  `_meta` are served byte-identically to before under the revision
  negotiated by `initialize`.
* New `server/discover` method (mandatory in 2026-07-28, and the
  backwards-compatibility probe): supported revisions of both eras,
  handler-gated capabilities, server identity and instructions.
* Declaring an unsupported revision returns
  `UnsupportedProtocolVersionError` (`-32022`) listing the supported set.
* A legacy client proposing `2026-07-28` via `initialize` negotiates down
  to `2025-11-25`: an initializing client is legacy by definition.
* Handlers can read the declared revision, client info and client
  capabilities from the `ClientContext`
  (`clientProtocolVersion`/`clientInfo`/`clientCapabilities`); the new
  `anonymousContext` builds an empty context.
* HTTP: modern requests get the 2026-07-28 request-metadata validation —
  the `MCP-Protocol-Version` header must match the body's declared
  revision, `Mcp-Method` must match the body method, and `Mcp-Name` must
  match `params.name`/`params.uri` for `tools/call`/`resources/read`/
  `prompts/get` (with `=?base64?…?=` sentinel decoding); violations return
  `400` with `HeaderMismatch` (`-32020`). Unknown methods return HTTP 404
  and unsupported revisions HTTP 400, so era-probing clients can
  distinguish them. Legacy requests keep the relaxed pre-2026 rules.

### Resource templates and completions

* Record constructors of a resource type now derive as resource /templates/
  (`UserProfile { userId :: Text }` →
  `resource://user_profile/{userId}`): the derived read handler matches
  template URIs, percent-decodes the path segments, and parses them into
  the constructor's (typed) fields. `deriveResourceTemplates` derives the
  `resources/templates/list` handler advertising them; the method carries
  the modern cacheability envelope.
* New `completions` handler slot serving `completion/complete` for prompt
  arguments and resource-template parameters (`CompletionRef`,
  `CompletionResult`, capped at 100 values per the spec). The
  `completions` capability is advertised automatically.
* `McpServerHandlers` gains `resourceTemplates` and `completions` fields;
  the new `noHandlers` value lets you construct handler sets by record
  update so future fields don't break your code.

### Typed tool arguments and results (BREAKING)

* Tool arguments arrive as full JSON values (`Map Text Value`). The Template
  Haskell derivation decodes records recursively and now supports **list
  fields**, **enumeration fields** (all-nullary data types, wired as string
  enums), and **nested record fields** in addition to the primitives.
  Primitive parsing is lenient: native JSON types or their string
  representations are both accepted (many clients send numbers/booleans as
  strings). Prompt arguments remain string-valued per the MCP specification.
* `inputSchema` is generated as a real JSON Schema (`Schema`/`SchemaType`
  ADT with `enum`, `items` and nested `object`s), replacing the flat
  `InputSchemaDefinition*` types that silently typed every non-primitive
  field as a string.
* Tool handlers produce a `ToolResult`: multiple content blocks,
  `structuredContent`, `_meta`, and `isError`. Tool *execution* failures
  should be reported via `isError` (see `toolError`) so the model can see
  them — per spec — instead of surfacing as JSON-RPC protocol errors.
  The `ToToolResult` class keeps simple handlers simple: returning
  `Content` or `Text` still works unchanged.
* Prompt handlers produce a `PromptResult` (optional description plus a
  multi-message conversation with user/assistant roles) via the analogous
  `ToPromptResult` class.
* `Content` gains `audio` and `resource_link` variants; embedded resources
  now carry their full contents as the spec requires.
* `ToolDefinition` gains `outputSchema`; `tools/call` responses carry
  `structuredContent`.
* Handler types are fixed to `IO` — the monad parameter was unusable
  through the public API (both transports required `IO`).

### Transport fixes

* stdio: a blank line on stdin no longer terminates the server, EOF shuts
  down cleanly instead of crashing, and malformed input is answered with
  proper JSON-RPC error responses (`-32700`/`-32600`, `id: null`).
* stdio: raw request bodies are no longer logged to stderr by default
  (tool arguments may carry sensitive data) — only message summaries.
  `runMcpServerStdioWithConfig` with `stdioVerbose = True` restores full
  body logging.
* JSON-RPC: messages are classified by shape (method/id presence) instead
  of parse-fallthrough, so a request with a malformed `id` is answered
  with an error rather than silently dropped as a notification. Request
  ids must be integral.
* HTTP: new `httpAllowedOrigins` policy on `HttpConfig` (Origin
  validation / DNS-rebinding protection, a spec MUST); accepted
  notifications return `202` with no body; malformed bodies get JSON-RPC
  error responses; the `Access-Control-Allow-Origin` header is set
  consistently on every response and echoes the validated origin (with
  `Vary: Origin`) when a policy is configured.
* HTTP (BREAKING): the non-spec GET "discovery" endpoint is removed — the
  MCP endpoint now answers GET with `405 Method Not Allowed`, matching the
  spec (no revision defines a GET discovery response, and `2026-07-28`
  requires 405 here).
* Integer tool arguments bound the scientific-notation exponent (1024, the
  same bound aeson uses) so a tiny payload like `1e1000000000` cannot force
  allocation of a gigabyte-sized `Integer`.

## 0.1.0.21 - ???

* **BREAKING**: every handler (prompt/resource/tool; list and get/read/call)
  now receives a `ClientContext` as its first argument, so a server can behave
  differently depending on who is calling. On stdio the context is anonymous;
  on HTTP it carries the request's bearer token and the principal returned by
  the authorization callback.
* **BREAKING**: `HttpConfig` gains an `httpAuthorize` field — an optional
  callback that validates the presented `Authorization: Bearer` token and
  returns an application-defined principal (`Nothing` rejects with 401). As it
  now holds a function, `HttpConfig` no longer derives `Show`/`Eq`.
* HTTP transport: accept requests without an `MCP-Protocol-Version` header
  (the spec says to assume `2025-03-26`), exempt `initialize` from the header
  check (it negotiates its version in the body), and keep rejecting a present
  but unsupported header with 400. Previously every request without the header
  was rejected, locking out pre-`2025-06-18` clients.
* `initialize` now advertises only the capabilities that actually have
  handlers, so strict clients no longer drop the server when e.g.
  `prompts/list` answers "not supported".
* CORS: preflight `OPTIONS` requests are exempt from authorization (browsers
  send no credentials on preflight) and `Authorization` is included in
  `Access-Control-Allow-Headers`.
* `http-simple-example` is now built with `-threaded`, which Warp requires;
  previously every request crashed with a `TimerManager` error.

## 0.1.0.20 - ???

* Fix protocol version negotiation: echo back any compatible revision the client
  proposes (`2024-11-05`, `2025-03-26`, `2025-06-18`, `2025-11-25`) instead of
  always responding with the server's own version. Fixes clients (e.g. Claude
  Code) that disconnect when they receive a different version than requested.
* Apply the same negotiation to the HTTP transport's `MCP-Protocol-Version`
  header check, which previously rejected anything other than `2025-06-18`.
* Default/fallback advertised version bumped to `2025-11-25`.

## 0.1.0.19 - ???

* Improve handler code generated by TemplateHaskell functions in `MCP.Server.Derive`:
    * Don't repeat `Map.fromList` for each argument in map lookup
    * Properly handle argument parse errors (Return `InvalidParams` error instead of crashing mcp server with `error`)

## 0.1.0.18 - 2026-02-09

* Switch default-language to GHC2021 to support broader range of GHC versions (9.6 - 9.12)

## 0.1.0.17 -- 2026-01-28

* Implement protocol version negotiation according to spec
* Remove unused dependencies, fix GHC warnings
* Add tested-with and haskell-ci generated GitHub Actions config

## 0.1.0.16 -- 2026-01-19

* Bump template-haskell dependency upper bound

## 0.1.0.15 -- 2025-08-13

* Update to MCP spec 2025-06-18

## 0.1.0.14 -- 2025-06-26

* Bump version bounds before adding to Stackage
* Remove support for JSON-RPC batching

## 0.1.0.13 -- 2025-06-17

* Better handling of UTF-8 in logs

## 0.1.0.12 -- 2025-06-17

* Fix unicode handling
* Refactor transports to remove unneeded functions
* Add unicode handling tests

## 0.1.0.11 -- 2025-06-17

* Refactor transports and add HTTP streaming support
* Add `MCP.Server.Handlers` module
* Add `MCP.Server.Transport.Http` and `MCP.Server.Transport.Stdio` modules

## 0.1.0.10 -- 2025-06-13

* Fix resources handling

## 0.1.0.9 -- 2025-06-13

* Bump versions of dependencies
* Port tests to hspec

## 0.1.0.8 -- 2025-06-12

* Support for nestable data types

## 0.1.0.7 -- 2025-06-09

* Documentation updates

## 0.1.0.6 -- 2025-06-09

* Remove pagination support

## 0.1.0.5 -- 2025-06-09

* Add descriptions to constructors and fields

## 0.1.0.4 -- 2025-06-09

* Clean up build configuration

## 0.1.0.3 -- 2025-06-09

* Refactor example modules
* Fix JSON to Haskell type conversion

## 0.1.0.0 -- 2025-06-05

* First version. Released on an unsuspecting world.
