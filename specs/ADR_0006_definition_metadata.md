# ADR 0006: Tool annotations, icons, content annotations

- **Status**: Landed (0.2.0.1)
- **Date**: 2026-08-01
- **Depends on**: —

## Context

Three metadata features from recent protocol revisions are missing from our
definition and content types:

- **Tool annotations** (2025-03-26): `readOnlyHint`, `destructiveHint`,
  `idempotentHint`, `openWorldHint` on tool definitions. Clients use these
  for permission UX (e.g. auto-approving read-only tools), so their absence
  degrades how our servers are treated.
- **Icons** (2025-11-25): optional icon lists on tool/prompt/resource
  definitions.
- **Content annotations** (2025-03-26): `audience`, `priority`,
  `lastModified` on content blocks. The 0.2.0 plan named these ("Content
  grows audio/resource-link/annotations") but the annotations part did not
  land.

## Decision

- Add an `Annotations` record to `Content` variants (optional field,
  omitted from JSON when absent) and `ToolAnnotations`/icon fields to the
  definition types, serialized per spec.
- Extend the derive customization API: alongside the existing
  `[("Constructor", "Description")]` mechanism, accept per-constructor
  options (a small record — working name `ToolOptions` — carrying
  annotations, icon, title) via a `WithOptions` variant. The plain
  string-pair API stays as-is.

## Consequences

- Cheap, additive, PVP minor; a natural companion to ADR_0005 in the same
  release batch.
- The `WithOptions` design decides the shape of all future per-constructor
  customization — worth a moment of API design so we don't accrete N
  `WithX` variants.
- Legacy golden fixtures are unaffected (new fields are omitted when
  unset).

## References

- MCP 2025-03-26 changelog (ToolAnnotations, content annotations)
- MCP 2025-11-25 changelog (icons)
