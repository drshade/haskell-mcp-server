# ADR 0008: Cancellation of in-flight requests

- **Status**: Landed (0.2.0.1)
- **Date**: 2026-08-01
- **Depends on**: ADR_0007

## Context

Handlers currently run synchronously to completion. `notifications/cancelled`
on stdio is honored only for subscription streams; a cancelled long-running
tool call still burns to the end, and on HTTP a client closing the response
stream (the modern cancellation signal — servers SHOULD stop work and MUST
NOT send further messages for the request) goes unnoticed.

Alone this is plumbing without payoff: until ADR_0007 moves handler
execution into an async/streaming model, there is nothing to interrupt. It
only makes sense on top of that structure — which is why it is a separate
ADR sequenced after it.

## Decision

- Run request handlers in `async` tasks (new dependency or plain
  `forkIO`+`MVar`).
- **stdio**: track in-flight requests by id; `notifications/cancelled`
  cancels the async, and nothing further is written for that id.
- **HTTP**: detect response-stream closure (WAI write failure / Warp
  connection teardown) and cancel the handler's async.
- Handlers opt into cooperative interruption simply by being interruptible
  Haskell code (blocking calls, `IO`); we document that `killThread`-style
  async exceptions are the mechanism, and that handlers needing cleanup
  should use `bracket`.

## Consequences

- Concurrency surface grows: responses may now interleave differently on
  stdio; the write lock and per-request bookkeeping need the same care as
  the subscription registry.
- Async-exception safety becomes part of the handler contract and must be
  documented prominently.
- Spec compliance: "SHOULD stop work as soon as practical, MUST NOT send
  any further messages for it" becomes true instead of aspirational.

## References

- Cancellation pattern: /specification/2026-07-28/basic/patterns/cancellation
- Streamable HTTP: stream closure as cancellation (2026-07-28)
