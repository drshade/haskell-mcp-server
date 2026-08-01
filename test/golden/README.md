# MCP wire-format conformance corpus

Each case in this directory is a pair of files:

- `<name>.request.json` — a single JSON-RPC request, exactly as it would
  arrive on the wire (one line).
- `<name>.response.json` — the reference server's exact response.

`manifest.json` enumerates the cases and states, per case, which
reference-server variant answers it (`handlers`) and whether the serving
transport can deliver change notifications (`notifications`). Responses are
compared as **parsed JSON** (structural equality), not raw bytes, so object
key order never matters — with one deliberate exception: JSON embedded
*inside a string* (the structured-output text block) is compared as part of
the string, so those bytes are canonical: compact, object keys sorted.

The corpus is deliberately **API-agnostic**: nothing in it refers to this
library's types or Haskell at all. Any MCP server implementation that
reproduces the reference server below can replay the requests and diff the
responses — the cases then pin protocol behavior (error codes, era
envelopes, capability advertisement) rather than any particular API.

## Eras

- `legacy/` — requests carry no modern `_meta`; they are answered under the
  revision negotiated by `initialize` (2024-11-05 … 2025-11-25 share this
  wire format for the operations covered here). These fixtures were
  generated from mcp-server v0.2.0 and are the anchor for "newer protocol
  work leaves legacy responses unchanged" — **never regenerate them** from a
  branch that intends to preserve legacy output.
- `modern/` — requests declare a revision (2026-07-28) in params `_meta`;
  responses carry the modern envelope: `resultType`, server identity in
  result `_meta`, and `ttlMs`/`cacheScope` on the cacheable methods.

## The reference server

Identity: name `Golden Server`, version `1.0.0`, instructions
`Golden fixture server`. Cache hints: `ttlMs` 0, scope `private`.

The `base` handler set:

| Feature | Definition | Behavior |
|---|---|---|
| Tool `echo` | input schema: object, required `text` (string, described "The text") | returns one text content block `echo: <text>` |
| Tool `boom` | not listed | any call returns `isError: true` with text `kaboom` |
| Prompt `greet` | one required argument `name` ("Who to greet") | description `A greeting`, one user message `Hello, <name>!` |
| Resource `resource://info` | name `info`, description `Some info`, `text/plain` | text contents `The golden info` |

The `extended` handler set adds (used only for methods that postdate the
v0.2.0 anchor, so the anchored capability fixtures stay untouched):

| Feature | Definition | Behavior |
|---|---|---|
| Resource template `resource://item/{itemId}` | name `item`, description `An item`, `text/plain` | — |
| Completions | any ref/argument | values = `["alpha", "beta"]` filtered by prefix of the partial value |
| Tool `echo_structured` | input schema: object, required `input` (string, "The text"); output schema: object, required `echoedText` (string, "The echoed text") and `echoedLength` (integer, "Its length"); description `Echo with structured output` | returns `structuredContent` `{"echoedText": <input>, "echoedLength": <length>}` plus one text content block containing the same JSON |
| Tool `annotated_probe` | input schema: object, required `probe` (string, "What to probe"); description `A read-only probe`; title `Probe`; annotations `readOnlyHint`/`idempotentHint` true; one icon `https://example.com/probe.png` | returns one text block `probed: <probe>` annotated with audience `["user"]`, priority `0.5` |

Cases with `"notifications": true` are answered as if the transport can
deliver change notifications (stdio with a configured notifier: legacy push
and modern `subscriptions/listen`), which flips the advertised
`listChanged`/`subscribe` capability flags.

## Adding a case

Write the `.request.json` by hand, add a manifest entry, and run the test
suite with `GOLDEN_ACCEPT=1`: the missing `.response.json` is written from
the current implementation's output. Existing response fixtures are never
overwritten — delete one first to regenerate it deliberately.
