# Roadmap

Where mcp-server is headed, as an ordering over the open
[ADRs](README.md#index). Dates are deliberately absent — releases are
batched (no release per feature), and the order reflects dependency and
value, not commitment.

What has shipped, and in which release, is in the
[CHANGELOG](../CHANGELOG.md); this file only looks forward.

## Next — interactivity

1. [ADR_0009 — MRTR: handlers that ask for input](ADR_0009_mrtr_input_required.md).
   The largest missing capability: tools, prompts and resources that ask
   the user for input mid-call, the 2026-07-28 replacement for
   elicitation. Derived handlers gain `Ask` and derived forms; the
   low-level handler types change, so other result-shape changes should
   be designed with it.

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

- **Roots, sampling, client-directed logging** — formally deprecated in
  2026-07-28; stderr (stdio) is the blessed logging channel and we already
  use it.
- **Legacy `resources/subscribe`** — superseded by `subscriptions/listen`;
  we advertise `subscribe` only to modern clients.
- **JSON-RPC batching and SSE resumability** — removed from the spec.
- **A per-feature release cadence** — releases are batched; CHANGELOG
  entries carry `???` dates until the Hackage upload stamps them.
