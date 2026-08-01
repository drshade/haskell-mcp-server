# ADR 0013: Extracting the conformance corpus

- **Status**: Hold — until there is a second consumer (see issue #9)
- **Date**: 2026-08-01
- **Depends on**: —

## Context

Since 0.2.1.0 the golden fixtures under `test/golden/` are a
self-describing, API-agnostic conformance corpus: on-disk
request/response pairs per protocol era, enumerated by a manifest, with
the reference server documented in prose. Nothing in it is
Haskell-specific — any MCP server implementation that reproduces the
reference server can replay the requests and diff the responses.

Issue #9 (merge with dpella/mcp?) surfaced the appetite for shared
infrastructure between competing implementations ("at least sharing
mcp-types… agreeing on something like wai"). A neutral conformance corpus
is the cheapest such shared ground: it standardizes observable wire
behavior without forcing anyone's API or types on anyone else.

## Decision (when taken up)

Extract the corpus to its own repository with per-revision directories,
this library's CI consuming it as a pinned submodule/tarball, and an
invitation to dpella/mcp (and others) to run it in their CI. Grow cases as
features land here (structured output, MRTR retry cycles, progress
ordering).

Not before there is a second consumer: extraction has real coordination
cost (versioning the corpus against spec revisions, governance of the
reference-server definition), and inside this repo the corpus already
delivers full value to us. The trigger is interest from another
implementation — at which point this is the concrete collaboration offer.

## Consequences

- Until extraction, keep the corpus scrupulously implementation-neutral
  (no Haskell-isms in fixtures, manifest, or README) so extraction stays a
  file move.

## References

- Issue #9; `test/golden/README.md`
