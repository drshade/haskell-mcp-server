# Roadmap

Where mcp-server is headed, as an ordering over the open
[ADRs](README.md#index). Dates are deliberately absent — releases are
batched (no release per feature), and the order reflects dependency and
value, not commitment.

**Current state**: 0.2.0.0 is on Hackage (typed core, dual-era protocol
support for 2024-11-05…2025-11-25 via `initialize` and stateless
2026-07-28, resource templates, completions, change notifications with
`subscriptions/listen`) but is deprecated there in favor of the pending
line: 0.2.0.1 sits merged-but-unreleased (WAI application export,
conformance corpus, and Batch 1: derived output schemas, definition
metadata), soaking until released. 0.2.0.1 knowingly supersedes the
never-adopted 0.2.0.0 in place rather than burning a major version.

## Batch 1 — complete the typed core *(landed)*

Both landed in the pending 0.2.0.1.

1. [ADR_0005 — Derived output schemas and structured content](ADR_0005_derived_output_schemas.md) — **Landed**.
2. [ADR_0006 — Tool annotations, icons, content annotations](ADR_0006_definition_metadata.md) — **Landed**
   (the definition-datatype extensions are breaking; they ship anyway in
   0.2.0.1 because the only affected release, 0.2.0.0, is deprecated with
   zero adopters).

## Batch 2 — long-running tools

3. [ADR_0007 — Progress notifications and per-request SSE](ADR_0007_progress_notifications.md).
   The biggest behavioral gap vs the spec; also the structural change
   (async, streaming HTTP responses) everything after it builds on.
4. [ADR_0008 — Cancellation of in-flight requests](ADR_0008_request_cancellation.md).
   Only meaningful on top of ADR_0007's execution model — sequenced
   immediately after, possibly the same release.

## Batch 3 — interactivity

5. [ADR_0009 — MRTR: input_required results](ADR_0009_mrtr_input_required.md).
   The largest missing capability class (tools that ask the user for
   input mid-call, the 2026-07-28 replacement for elicitation). Headline
   of its own release; touches result types, so any result-shape changes
   should co-design with it.

## Opportunistic — order by demand

- [ADR_0010 — OAuth protected-resource metadata](ADR_0010_oauth_resource_metadata.md):
  do when an HTTP-deployment user needs discoverable auth; closes the
  practical gap with dpella/mcp.
- [ADR_0011 — Pagination and the extensions capability](ADR_0011_protocol_completeness.md):
  do when someone has enough tools/resources to care, or as the
  prerequisite step for the tasks extension.

## On hold, with stated triggers

- [ADR_0012 — Tasks extension](ADR_0012_tasks_extension.md): wait for the
  extension spec to stop moving and for Batch 2 to land.
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
