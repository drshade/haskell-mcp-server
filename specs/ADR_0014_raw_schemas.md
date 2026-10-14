# ADR 0014: Raw JSON Schema keywords

- **Status**: Proposed
- **Date**: 2026-10-13
- **Depends on**: —

## Context

Tool input and output schemas are values of `Schema`, whose `SchemaType`
covers strings (optionally an enum), integers, numbers, booleans, arrays
and objects with required properties. That is all the derivation in
`MCP.Server.Derive` produces, but it is a small part of JSON Schema, which
the specification uses for `inputSchema` and `outputSchema`. A server that
describes its arguments some other way — hand-written schemas, or schemas
derived by another library to match its `aeson` instances — cannot say:

- that a value may be `null`;
- that a value is one of several shapes (`anyOf`, as for sum types);
- that a shape is recursive (`$defs` and `$ref`);
- numeric bounds, string lengths or patterns, array lengths, or
  `additionalProperties`.

Such servers must either drop information, so that clients send arguments
the server then rejects, or bypass the typed definitions altogether, which
the handler API does not allow: `tools/list` is always built from
`ToolDefinition` values.

## Decision

Add an escape hatch to `SchemaType`:

```haskell
  | SchemaRaw Object
    -- ^ JSON Schema keywords, serialized as given.
```

It serializes as its keywords; a `schemaDescription` alongside it is added
as `description`, replacing one among the keywords. Raw schemas can appear
anywhere a `Schema` can, including inside `SchemaObject` and `SchemaArray`.
The library does not interpret or validate the keywords. Like
`ToolOutputRaw`, it is for callers who need more than the typed API offers.

## Consequences

- Servers can advertise any JSON Schema without leaving the typed handler
  API, and the derived schemas are unchanged.
- Adding a constructor breaks exhaustive matches on `SchemaType` in
  downstream code, so under the PVP this needs a major version bump (or
  rides along with the next one).
- Validity is the caller's responsibility: the specification requires a
  tool's `inputSchema` to describe an object, and nothing checks that raw
  keywords do.
- Code that inspects schemas (such as test helpers reading a property's
  type) must handle a shape it cannot see into.

## References

- MCP specification, `Tool.inputSchema` and `Tool.outputSchema`
- JSON Schema 2020-12
