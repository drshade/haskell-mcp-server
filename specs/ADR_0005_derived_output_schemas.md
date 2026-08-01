# ADR 0005: Derived output schemas and structured content

- **Status**: Landed (0.3.0.0, née 0.2.1.0 — see CHANGELOG)
- **Date**: 2026-08-01
- **Depends on**: —

## Context

Since 0.2.0 the protocol layer carries `outputSchema` on `ToolDefinition`
and `structuredContent` on tool results, but only manual handlers can
populate them — the Template Haskell derivation always emits
`toolDefinitionOutputSchema = Nothing`. Meanwhile the library's entire
thesis is that schemas fall out of ADTs: `inputSchema` is already derived
from the tool constructor's fields.

The spec (2025-06-18+) also recommends that when a result carries
`structuredContent`, the serialized JSON is additionally included as a text
content block for clients that predate structured output.

## Decision

Extend the derive layer so a tool handler can return a typed result and get
the full structured-output story for free:

- A new derivation (working name `deriveToolHandlerWithOutput`, or an
  output-type annotation on the existing one) maps the handler's result
  record to a generated `outputSchema` using the same `Schema` machinery as
  input derivation (primitives, `Maybe`, lists, all-nullary enums, nested
  records).
- At call time the typed result is serialized into `structuredContent`, and
  the library also appends the spec-recommended text block containing the
  same JSON, unless the handler supplied its own content.
- Plain `ToToolResult` handlers are untouched — this is opt-in per tool.

## Consequences

- Completes the typed core end-to-end (typed in, typed out); no other MCP
  library derives both sides declaratively.
- New exports → PVP minor bump; can join the pending 0.2.1.x line.
- The schema generator is shared with input derivation, so schema-feature
  work (e.g. richer JSON Schema keywords) benefits both directions.
- Golden corpus gains structured-output cases in both eras.

## References

- MCP 2025-06-18 changelog (structured tool output, `outputSchema`)
- 2026-07-28 loosened `outputSchema` to any JSON Schema 2020-12 keywords
  (SEP-2106)
