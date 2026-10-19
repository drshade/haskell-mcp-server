# ADR 0009: MRTR — handlers that ask for input

- **Status**: Accepted
- **Date**: 2026-10-16 (first drafted 2026-08-01)
- **Depends on**: —

## Context

Revision 2026-07-28 replaced server-initiated requests (elicitation,
sampling, roots) with Multi Round-Trip Requests. A server that needs
input answers the client's request with an `InputRequiredResult`
(`resultType: "input_required"`) instead of a final result:

- `inputRequests` maps server-chosen keys to requests the client must
  fulfil: `elicitation/create`, `sampling/createMessage` or `roots/list`;
- `requestState` is an opaque string the client must echo back unchanged.

The client gathers the input and retries the original request with a new
JSON-RPC id, carrying `inputResponses` (same keys) and the `requestState`.
The server completes, or asks again. Every round is an independent
request: nothing ties a retry to the process that saw the previous round.

Constraints from the specification:

- Only `tools/call`, `prompts/get` and `resources/read` may return
  `input_required`; no other method may.
- A server must not send an input request the client has not declared a
  capability for. Elicitation has two modes, `form` (a flat schema of
  primitive fields) and `url` (completed out of band), declared
  separately under `elicitation`.
- `requestState` is attacker-controlled input. If it influences
  authorization or logic, its integrity must be protected (the spec
  suggests HMAC or AEAD), and it should bind the principal, an expiry and
  the originating request, to limit replay. Single-use guarantees are the
  server's job.
- A retry carries only the latest round's `inputResponses`. Anything
  from earlier rounds the server still needs must travel in
  `requestState`.
- Sampling and roots are deprecated in 2026-07-28 (earliest removal in
  the first revision on or after 2027-07-28). Elicitation is not.

Today a handler cannot ask anything mid-call. This is the largest
spec-current capability we lack.

### Why handlers cannot simply suspend

Haskell could pause a handler at the point it asks a question: block its
thread on an `MVar`, or capture the rest of the computation as a
continuation. But the paused computation lives in the memory of one
process, and the retry may reach another instance, arrive after a
restart, or never arrive at all (servers must not assume a retry).
Continuations over `IO` also cannot be serialized into `requestState`.
Suspension therefore needs sticky routing and timeouts for abandoned
handlers — exactly the shared state MRTR is designed to avoid.

## Decision

The derived (Template Haskell) interface is the one users write against,
so the design starts there: a derived handler can return `Ask` instead of
`IO`, and ask questions whose forms are derived from records. Underneath
sit an `Ask` monad that turns multi-round handlers into straight-line
code by **journaled replay**, and a low-level result type that maps
directly onto the wire. The library owns `requestState` throughout.

### Derived handlers and forms

Today a derived tool handler has the type
`ClientContext -> MyTool -> IO r`. It may now also return `Ask r`:

```haskell
data Environment = Staging | Production

data DeployTarget = DeployTarget
  { environment :: Environment
  , reason      :: Text
  }

data Confirm = Confirm { proceed :: Bool }

data OpsTool = Deploy { service :: Text }

$(deriveForm ''DeployTarget)
$(deriveForm ''Confirm)

handleTool :: ClientContext -> OpsTool -> Ask Content
handleTool _ (Deploy svc) = do
  target <- elicit ("Where should " <> svc <> " deploy to?")
  case target of
    Accepted t -> do
      plan <- step (planDeploy svc (environment t))
      ok   <- elicit ("Apply " <> describe plan <> "?")
      case ok of
        Accepted (Confirm True) -> ContentText <$> step (applyPlan plan)
        _                       -> pure (ContentText "Deployment cancelled")
    _ -> pure (ContentText "No target chosen")

main = runMcpServerStdio info noHandlers
  { tools = Just $(deriveToolHandler ''OpsTool 'handleTool) }
```

- `deriveForm ''T` generates an instance of `class Form a` (the flat
  elicitation schema and a parser for the accepted `content`) from a
  record of strings, numbers, booleans, enumerations, optional fields and
  lists of enumeration values: what elicitation allows. A nested record
  or other unsupported field is a compile-time error naming the field.
  `deriveFormWithDescription` takes field descriptions as the other
  derive functions do.
- `elicit :: Form a => Text -> Ask (Answer a)` picks the form from the
  result type, so a question is one line.
- The `derive*Handler` splices accept handlers in `IO` or `Ask`, through
  a small class with an instance for each, so existing handlers compile
  unchanged and switching one to `Ask` changes only its type.
- `deriveToolHandler`, `derivePromptHandler` and `deriveResourceHandler`
  all support it, since the spec allows `input_required` on
  `tools/call`, `prompts/get` and `resources/read`.

### Low level: `Outcome`

Hand-written handlers, and the code the derive functions generate, use
an outcome type. The three call handlers return it:

```haskell
data Outcome a
  = Complete a
  | InputRequired (Map Text InputRequest) (Maybe Text)
    -- ^ Requests keyed by handler-chosen ids, and the handler's own state

type ToolCallHandler     = ClientContext -> ToolName -> Map Text Value -> IO (Either Error (Outcome ToolResult))
type PromptGetHandler    = ClientContext -> PromptName -> Map Text Text -> IO (Either Error (Outcome PromptResult))
type ResourceReadHandler = ClientContext -> URI -> IO (Either Error (Outcome ResourceContent))
```

```haskell
data InputRequest
  = ElicitForm Text Schema          -- ^ message, requested schema (flat object)
  | ElicitUrl Text URI              -- ^ message, URL to visit
  | RawInputRequest Text Value      -- ^ method and params, e.g. sampling or roots

data InputResponse
  = ElicitResponse ElicitAction (Maybe (Map Text Value))
  | RawInputResponse Value

data ElicitAction = Accept | Decline | Cancel
```

On a retry, `ClientContext` carries the decoded `inputResponses` and the
handler's own state from the previous round. The handler's state is a
`Text` of its choosing; the library wraps it (see *State protection*), so
low-level handlers get integrity, binding and expiry without doing
anything.

Elicitation is typed. Sampling and roots are deprecated, so they get only
the untyped `RawInputRequest` / `RawInputResponse` pass-through.

### High level: the `Ask` monad

```haskell
data Answer a = Accepted a | Declined | Cancelled | Unavailable

elicit    :: Form a => Text -> Ask (Answer a)
elicitUrl :: Text -> URI -> Ask (Answer ())
step      :: (ToJSON a, FromJSON a) => IO a -> Ask a
askContext :: Ask ClientContext
instance MonadIO Ask   -- plain liftIO: not journaled, runs every round

runAsk :: Ask a -> ClientContext -> IO (Either Error (Outcome a))
```

`Form a` is usually derived with `deriveForm` (above). It can also be
written by hand, as a schema plus a parser, for forms a record cannot
describe.

The library runs the handler from the start on every round, against a
**journal** of what earlier rounds produced:

- Each `elicit` and each `step` takes the next position in the journal.
- An `elicit` whose position holds an answer returns it immediately. The
  first `elicit` without one stops the run, and the library answers
  `input_required` with that one request, keyed by its position.
- A `step` whose position holds a result returns it without running the
  action. Otherwise it runs the action and appends the result.
- When the handler finishes, the result is `Complete`.

The journal travels in `requestState`, so `step` lets IO sit between
questions: it runs once and is replayed from the journal afterwards.
Each journal entry records a fingerprint of its question (message and
schema) or its kind. If replay reaches a position whose entry does not
match — the handler asked something different, or in a different order —
the run fails with an internal error that names the position, rather than
handing an answer to the wrong question.

Rules for handler authors, stated in the documentation:

- **`step` is at-least-once.** If a round's reply is lost and the client
  retries with the previous state, the step runs again. Effects that must
  happen once (payments, emails) must be idempotent or guarded
  server-side, as the spec also requires for single-use state.
- **`step` results travel to the client and back on every later round.**
  Return small values (an id, not the object) and keep them serializable.
- **Plain `liftIO` re-runs on every round.** It is for reads whose
  results may differ between rounds without changing which questions are
  asked.

One question per round in this version. `Applicative` batching of
independent questions into one `inputRequests` map is a possible later
addition.

### Clients that cannot answer

`elicit` returns `Unavailable`, without a round trip, when the client
cannot answer:

- legacy (≤ 2025-11-25) clients: our transports never send
  server-initiated requests, so even an `elicitation` capability from
  `initialize` does not help;
- modern clients that did not declare `elicitation`, or not the mode in
  question (`form` or `url`).

The handler decides what to do: fall back to a default, or fail with a
tool error. At the low level, a handler that returns `InputRequired` with
a request the client cannot handle is a server bug, answered with
`-32603`.

### State protection

The library owns the `requestState` wire value. Its payload is:

- a format version;
- the authenticated principal (`clientPrincipal`), or none;
- the method and a SHA-256 digest of the request's parameters, excluding
  `inputResponses`, `requestState` and `_meta`;
- an expiry;
- the `Ask` journal, or the low-level handler's own state.

It is serialized, encrypted and authenticated with an AEAD
(ChaCha20-Poly1305 with a random nonce), and base64url-encoded with a key
id prefix for rotation. Encryption, not just a signature, because `step`
results may hold internal data the client should not read.

On a retry the library verifies, decrypts and checks the principal,
method, parameter digest and expiry before the handler runs. Any failure
is answered with `-32602`; the handler never sees rejected state. The
message is generic, except that expired state says so (the client should
start over), and state under an unknown key id says it was issued by
another instance or a retired key.

There is no compression: compressing attacker-chosen answers together
with secret `step` results before encryption would leak information
through ciphertext length. There is no size limit either; the
specification sets none, and the size is under the handler author's
control.

Configuration is a `StateCodec` in the transport configurations:

```haskell
data StateCodec = StateCodec
  { sealState     :: ByteString -> IO Text
  , openState     :: Text -> IO (Either StateError ByteString)
  , stateLifetime :: NominalDiffTime
  }

aeadStateCodec      :: StateKey -> [StateKey] -> StateCodec   -- seal with the first, open with any
ephemeralStateCodec :: IO StateCodec                          -- random key per process
```

The library builds and checks the payload; a codec only seals and opens
bytes, so the payload format stays private to the library and a
replacement codec (a key service, a server-side store) cannot get the
binding checks wrong.

The default is `ephemeralStateCodec`. That is correct for stdio and for a
single HTTP instance; multi-instance HTTP deployments must share a key
through `aeadStateCodec`, and the HTTP documentation says so. Replacing
the codec is the override, for example a codec that keeps journals in a
server-side store and puts only a key in `requestState`. The default
expiry is 60 minutes, configurable; the spec asks only for a "short"
expiry, and a user may take a while to answer a form.

### Wire and eras

- `input_required` exists only for modern (2026-07-28+) requests; dispatch
  stamps `resultType: "input_required"` with `inputRequests` and
  `requestState` on the modern envelope.
- Legacy clients never receive it. `Unavailable` covers the `Ask` path,
  and the `-32603` rule covers the low-level path.
- The sealed state records the keys of the requests it was issued with.
  A retry's `inputResponses` are read only for those keys; unknown keys
  are ignored, as the spec asks, and answers sent without a state are
  ignored altogether, so a client cannot volunteer answers to questions
  the handler never asked. A malformed answer to an issued request, or an
  `inputResponses` that is not an object, is `-32602`. A missing answer
  just leaves that position unanswered, so the handler asks again.
- Progress and cancellation work per round; each round is its own request.

### Phasing

1. Wire types, `Outcome`, the state codec and capability checks, with
   golden fixtures for full cycles (call → `input_required` → retry →
   result) and for rejected state (tampered, expired, other principal,
   other arguments).
2. The `Ask` monad, `step` and journal fingerprints.
3. The derived interface: `Ask` handlers in the `derive*Handler` splices,
   and `deriveForm`, reusing the schema machinery from ADR_0005.

All three ship together: the derived interface is what users will use,
so the feature is not done until phase 3 lands. The version number is
decided at release time.

### Deferred

- Suspension mode: keep paused handlers in memory for stdio and
  single-instance HTTP, allowing unjournaled IO between questions. Only
  if replay proves too limiting in practice.
- Batching independent questions into one round.
- Typed sampling and roots; both are deprecated.

## Consequences

- Derived handlers are unaffected: existing `IO` handlers compile
  unchanged, and `Ask` is opt-in per handler.
- Hand-written call handlers change type to return `Outcome`, and
  migrate by wrapping results in `Complete`. `ClientContext` and the
  transport configurations gain fields. This is breaking under the PVP;
  we accept it for the cleaner design, since the derived interface is
  the one in use.
- New dependency: `crypton` for the AEAD, random nonces and SHA-256.
- Handlers written with `Ask` read like single-round code. In exchange
  they must keep their questions deterministic, wrap IO that must not
  repeat in `step`, and treat `step` as at-least-once. The documentation
  carries these rules with examples.
- `requestState` can be neither read nor altered by clients, and is bound
  to principal, request and expiry by default, which meets the spec's
  MUSTs without handler code.
- Multi-instance HTTP deployments must configure a shared key. With the
  per-process default, a retry that reaches another instance fails with
  an error saying the state came from another instance, rather than
  misbehaving silently.
- `input_required` is modern-only, another place the dual-era split must
  stay honest.

## References

- MCP 2026-07-28: Multi Round-Trip Requests
  (`/specification/2026-07-28/basic/patterns/mrtr`, SEP-2322)
- MCP 2026-07-28: Elicitation (`/specification/2026-07-28/client/elicitation`)
- MCP 2026-07-28: deprecated features registry (SEP-2577: sampling, roots)
- Schema 2026-07-28: `InputRequiredResult`, `InputResponseRequestParams`,
  `ElicitRequestFormParams`, `ElicitRequestURLParams`, `ElicitResult`,
  `ClientCapabilities.elicitation`
- Journaled replay as used by durable-execution engines (Temporal,
  Azure Durable Functions)
