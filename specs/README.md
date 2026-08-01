# Architecture Decision Records

Design decisions for mcp-server live here as numbered ADRs. Each records
the context, the decision, and its consequences — so future readers (and
future maintainers) can see not just what the library does but why.

[ROADMAP.md](ROADMAP.md) orders the open ADRs into an intended sequence.

## Statuses

- **Proposed** — analysed and written up; not yet committed to.
- **Accepted** — we intend to build this; design is settled enough to start.
- **Landed** — implemented and merged (the ADR notes the version/PR).
- **Hold** — deliberately parked, with the unblocking condition stated.
- **Historical** — ADR_0001–0004 predate this convention: working notes
  from earlier development, preserved as-written and renumbered. They are
  not templates for new ADRs.

## Format for new ADRs

Filename `ADR_XXXX_short_slug.md`, numbered sequentially. Body:

```markdown
# ADR XXXX: Title

- **Status**: Proposed | Accepted | Landed (vX.Y.Z) | Hold
- **Date**: YYYY-MM-DD
- **Depends on**: ADR_XXXX (or —)

## Context
## Decision
## Consequences
## References
```

## Index

| ADR | Title | Status |
|---|---|---|
| [0001](ADR_0001_original_spec.md) | Original library specification | Historical |
| [0002](ADR_0002_support_parameter_types.md) | Support for better parameter types | Historical |
| [0003](ADR_0003_test_improvements.md) | Test suite improvements | Historical |
| [0004](ADR_0004_upgrade_to_2025-06-18.md) | Upgrade to protocol 2025-06-18 | Historical |
| [0005](ADR_0005_derived_output_schemas.md) | Derived output schemas and structured content | Proposed |
| [0006](ADR_0006_definition_metadata.md) | Tool annotations, icons, content annotations | Proposed |
| [0007](ADR_0007_progress_notifications.md) | Progress notifications and per-request SSE | Proposed |
| [0008](ADR_0008_request_cancellation.md) | Cancellation of in-flight requests | Proposed |
| [0009](ADR_0009_mrtr_input_required.md) | MRTR: input_required results (elicitation) | Proposed |
| [0010](ADR_0010_oauth_resource_metadata.md) | OAuth protected-resource metadata | Proposed |
| [0011](ADR_0011_protocol_completeness.md) | Pagination and the extensions capability | Proposed |
| [0012](ADR_0012_tasks_extension.md) | Tasks extension | Hold |
| [0013](ADR_0013_conformance_corpus_extraction.md) | Extracting the conformance corpus | Hold |
