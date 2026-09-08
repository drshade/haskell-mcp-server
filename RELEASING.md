# Releasing

Releases are batched: merged work soaks on `main` under a pending version
whose CHANGELOG entry reads `## X.Y.Z.W - ???` until it ships. Pushing a
version tag publishes that version to Hackage. The tag is the release, so
the checks happen before it: in the release-prep PR, and again in the
workflow's guards.

## Procedure

1. **Release-prep PR.** On a branch, stamp the date on the pending
   CHANGELOG entry (`## 0.2.0.3 - 2026-10-01`) and update the "Current
   state" paragraph in `specs/ROADMAP.md`. The version in
   `mcp-server.cabal` should already be right; check it follows the PVP
   comment block there. Review this PR as the release review: once the
   tag is pushed there is no further checkpoint. Merge it.

2. **Tag.** On the merged `main`:

   ```bash
   git tag v0.2.0.3
   git push origin v0.2.0.3
   ```

   The tag must be `v` followed by the exact cabal version.

3. **Wait for the `Hackage release` workflow.** It refuses to proceed if
   the tag and cabal version disagree, or if the CHANGELOG entry for that
   version is still `???`. Otherwise it runs `cabal check`, builds and
   tests the source distribution in isolation, builds the documentation
   tarball with `--haddock-for-hackage`, and publishes both. The version
   is public at

   ```
   https://hackage.haskell.org/package/mcp-server-<version>
   ```

   within a few minutes. Check that the README rendered and that the
   module documentation is present.

Hackage versions cannot be deleted, only deprecated. If a release turns
out to be broken, deprecate it on Hackage and ship a fixed version.

## Setup (once)

The workflow needs a `HACKAGE_AUTH_TOKEN` repository secret. Generate a
token on the Hackage account management page
(<https://hackage.haskell.org/users/account-management>) and add it under
the repository's *Settings → Secrets and variables → Actions*. The token
only needs to belong to a maintainer of the package.

## Doing it by hand

The workflow is a thin wrapper over standard cabal commands, so the same
release can be made locally if Actions is unavailable:

```bash
cabal check
cabal sdist --output-directory dist-release
cabal haddock --haddock-for-hackage --enable-doc
cabal upload --publish --token "$HACKAGE_AUTH_TOKEN" dist-release/mcp-server-<version>.tar.gz
cabal upload --publish --token "$HACKAGE_AUTH_TOKEN" --documentation dist-newstyle/mcp-server-<version>-docs.tar.gz
```

Drop `--publish` from both `upload` commands to upload a candidate instead,
which can be inspected at `.../package/mcp-server-<version>/candidate` and
promoted or deleted from there.
