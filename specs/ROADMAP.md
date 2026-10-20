# Roadmap

Where mcp-server is headed, as an ordering over the open
[ADRs](README.md#index). Dates are deliberately absent — releases are
batched (no release per feature), and the order reflects dependency and
value, not commitment.

What has shipped, and in which release, is in the
[CHANGELOG](../CHANGELOG.md); this file only looks forward.

## Opportunistic — order by demand

- [ADR_0010 — OAuth protected-resource metadata](ADR_0010_oauth_resource_metadata.md):
  do when an HTTP-deployment user needs discoverable auth; closes the
  practical gap with dpella/mcp.
- [ADR_0011 — Pagination and the extensions capability](ADR_0011_protocol_completeness.md):
  do when someone has enough tools/resources to care, or as the
  prerequisite step for the tasks extension.

## On hold, with stated triggers

- [ADR_0012 — Tasks extension](ADR_0012_tasks_extension.md): wait for the
  extension spec to stop moving.
- [ADR_0013 — Extracting the conformance corpus](ADR_0013_conformance_corpus_extraction.md):
  wait for a second consumer (issue #9 is the likely origin).

## Deliberate non-goals

- **Roots, sampling, session-level logging** — formally deprecated in
  2026-07-28. That covers `logging/setLevel` and the session-wide log
  stream it controlled; stderr (stdio) is the blessed channel for server
  logs. Per-request logging is not part of this: a request that declares
  `io.modelcontextprotocol/logLevel` gets `notifications/message` through
  `logToClient`, which is supported.
- **Legacy `resources/subscribe`** — superseded by `subscriptions/listen`;
  we advertise `subscribe` only to modern clients.
- **JSON-RPC batching and SSE resumability** — removed from the spec.
- **A per-feature release cadence** — releases are batched; CHANGELOG
  entries carry `???` dates until the release-prep PR stamps them.
