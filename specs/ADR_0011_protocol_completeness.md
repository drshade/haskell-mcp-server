# ADR 0011: Pagination and the extensions capability

- **Status**: Proposed
- **Date**: 2026-08-01
- **Depends on**: —

## Context

Two small spec features round out protocol completeness:

- **Pagination**: list operations accept a `cursor` param and may return
  `nextCursor`. Clients MUST support pagination; servers MAY paginate. We
  never paginate — fine for typical derived servers (a handful of tools),
  a real limitation for the manual-handler user with hundreds of resources.
- **Extensions capability** (2026-07-28): `ServerCapabilities` gained an
  `extensions` map (identifier → settings object) for negotiating optional
  extensions. We have no way to populate it, which blocks advertising any
  extension (including tasks, ADR_0012).

## Decision

- Add an opt-in page size to the list handlers' dispatch: when configured,
  the library slices the handler-returned list and mints opaque cursors
  (offset-based, encoded, treated as opaque by clients per spec). Handlers
  keep returning full lists — pagination is a dispatch concern, keeping the
  derive API untouched. (A streaming/chunked handler API is deliberately
  out of scope until someone needs it.)
- Add `serverExtensions :: Map Text Value` to server configuration,
  advertised in capabilities in both eras' capability objects.

## Consequences

- Additive, PVP minor; low urgency — schedule opportunistically or when a
  user asks.
- Cursor stability across list changes is documented as best-effort (the
  spec allows invalid-cursor errors: `-32602`).
- The `extensions` map is a prerequisite for ADR_0012.

## References

- Pagination: /specification/2026-07-28/server/utilities/pagination
- Extensions: 2026-07-28 changelog (minor change 1), versioning page
