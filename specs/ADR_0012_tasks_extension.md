# ADR 0012: Tasks extension

- **Status**: Hold — until the extension spec stabilizes and ADR_0007/0008
  have landed
- **Date**: 2026-08-01
- **Depends on**: ADR_0007, ADR_0008, ADR_0011 (extensions capability)

## Context

Revision 2026-07-28 moved experimental tasks out of the core protocol into
an official extension (`io.modelcontextprotocol/tasks`), redesigning it in
the process: blocking `tasks/result` replaced by polling `tasks/get`, a new
`tasks/update` for client→server input, `tasks/list` removed, and servers
may return task handles unsolicited. It is the spec's answer to
long-running tool calls that outlive a request/response cycle — a natural
fit alongside our notifier machinery.

## Decision (when taken up)

Implement the extension behind the `extensions` capability map
(ADR_0011): a task-aware tool handler returns a task handle; the library
stores task state (pluggable store, default in-memory) and serves
`tasks/get`/`tasks/update`. Not before: the extension was redesigned in
this very revision and may move again, and the async execution model from
ADR_0007/0008 is a prerequisite for actually running anything in the
background.

## Consequences

- Parked deliberately; revisit when the extension has survived a revision
  unchanged or a user asks for it.

## References

- 2026-07-28 changelog major change 6 (SEP-2663)
- Extension: /extensions/tasks/overview
