# ADR 0010: OAuth protected-resource metadata

- **Status**: Proposed
- **Date**: 2026-08-01
- **Depends on**: —

## Context

Our HTTP auth story is a deliberately minimal callback (`httpAuthorize`
receives the bearer token, returns a principal or rejects). The MCP
authorization spec expects a compliant HTTP server to also act as an OAuth
2.0 *protected resource*: serve RFC 9728 metadata at
`/.well-known/oauth-protected-resource` naming its authorization servers,
and return `401` with a `WWW-Authenticate` header pointing at that metadata
so clients can discover how to obtain a token. This discovery layer is what
lets MCP clients (Claude, inspector tooling) drive an OAuth flow
automatically — and it is the main functional advantage dpella/mcp
currently has (servant-auth JWT support).

## Decision

- Keep `httpAuthorize` as the validation primitive (the library still never
  validates tokens itself).
- Add an optional `httpResourceMetadata` config: when set (authorization
  server URLs, resource id, scopes), the transport serves the RFC 9728
  document at the well-known path and enriches `401` responses with the
  spec-shaped `WWW-Authenticate` header.
- Token validation guidance (JWT verification against the AS's JWKS) goes
  in documentation/examples rather than the library, avoiding a heavy
  crypto dependency footprint; a separate optional package could offer a
  ready-made JWT `httpAuthorize` later.

## Consequences

- Compliant discovery without hardcoding any particular token format; the
  library stays dependency-light.
- Closes the practical gap with dpella/mcp for browser/agent-driven auth.
- Needs care with `httpEndpoint` vs the well-known path when users embed
  `mcpApplication` in a larger router (document that the well-known path is
  served at the root, or expose it as a second WAI app).

## References

- MCP authorization spec (2025-06-18+), RFC 9728, RFC 8414
- 2026-07-28 auth changes: RFC 9207 `iss` validation, Client ID Metadata
  Documents (client-side; server impact is documentation only)
