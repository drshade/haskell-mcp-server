# ADR 0009: MRTR — input_required results (elicitation)

- **Status**: Proposed
- **Date**: 2026-08-01
- **Depends on**: —

## Context

Revision 2026-07-28 replaced server-initiated requests (elicitation,
sampling, roots) with the Multi Round-Trip Request pattern: a server that
needs client input returns an `InputRequiredResult`
(`resultType: "input_required"`) whose `inputRequests` carry what it needs
(e.g. an `elicitation/create` payload); the client gathers the input and
**retries the original request** with `inputResponses` attached, plus any
opaque `requestState` the server included for correlation.

We implement none of this — a tool handler cannot ask the user anything
mid-call. This is the largest missing capability class that is
spec-current. (Legacy `elicitation/create` as a server-initiated request is
deliberately out of scope: our transports do not send server-initiated
requests, and the feature is superseded.)

## Decision

- `ToolResult` (or a wrapping result type) gains an `input_required`
  variant carrying typed `inputRequests` and an opaque `requestState`.
- The handler API lets a tool either complete or request input; on retry,
  the handler receives the `inputResponses` and its own `requestState`
  back. Because the server is stateless, all correlation state rides in
  `requestState` — the library treats it as an opaque JSON value.
- Dispatch stamps `resultType: "input_required"` and the MRTR fields on the
  modern envelope; legacy clients never see the variant (a handler
  returning it to a legacy request gets a defined error, since legacy has
  no retry semantics).
- A TH nicety can follow later: derive the elicitation input schema from a
  record type, same machinery as ADR_0005.

## Consequences

- The headline feature of a future minor/major release; touches the result
  types, so design it before (or with) any other result-shape change.
- Interacts with the era model: input_required is modern-only, another
  place the dual-era split must stay honest.
- Golden corpus gains retry-cycle cases (initial → input_required → retry →
  complete).

## References

- MRTR pattern: /specification/2026-07-28/basic/patterns/mrtr (SEP-2322)
- 2026-07-28 changelog items 7–8 (InputRequiredResult, resultType)
