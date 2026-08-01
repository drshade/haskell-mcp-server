# ADR 0007: Progress notifications and per-request SSE

- **Status**: Landed (0.2.0.1)
- **Date**: 2026-08-01
- **Depends on**: —

## Context

A client may attach a `progressToken` to any request's `_meta`; the server
may then emit `notifications/progress` (progress, optional total, optional
message) while the request runs. We support none of this: handlers have no
way to report progress, and on HTTP every request is answered with a single
JSON object — whereas the Streamable HTTP spec expects request-scoped
notifications to flow on an SSE response stream for that request, which
clients MUST support. (Per-request `notifications/message` logging via the
`io.modelcontextprotocol/logLevel` `_meta` key rides the same mechanism and
can come along nearly for free.)

## Decision

- `ClientContext` gains a `reportProgress` action (progress → maybe total →
  maybe message → IO ()). It is a no-op when the request carried no
  `progressToken`, so handlers can call it unconditionally.
- **stdio**: progress notifications are written to the shared stdout
  channel (under the existing write lock), interleaved before the response
  — the spec's normal shape for stdio.
- **HTTP**: when a request carries a `progressToken`, the transport answers
  with `Content-Type: text/event-stream` and streams progress notifications
  followed by the final response, reusing the SSE plumbing built for
  `subscriptions/listen` (`X-Accel-Buffering: no`, keep-alive comments).
  Requests without a token keep the single-JSON-object response.
- Optionally in the same change: honor `io.modelcontextprotocol/logLevel`
  by giving `ClientContext` a client-log action with the same delivery
  rules (never emitted when the key is absent, per spec MUST NOT).

## Consequences

- Handler execution on HTTP moves inside a streaming response body; this is
  the structural change that ADR_0008 (cancellation) builds on — do this
  first.
- `ClientContext` grows; `anonymousContext` keeps construction stable.
- Golden corpus stays single-message per case; progress ordering gets its
  own streaming tests (like the live subscription driver).

## References

- MCP 2026-07-28 Streamable HTTP: per-request SSE response streams
- Progress pattern: /specification/2026-07-28/basic/patterns/progress
- Logging via `_meta` logLevel: 2026-07-28 changelog (SEP-2575)
