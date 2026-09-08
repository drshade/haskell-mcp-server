# Releasing

Releases are batched: merged work soaks on `main` under a pending version
whose CHANGELOG entry reads `## X.Y.Z.W - ???` until it ships. Nothing is
published automatically. Pushing a version tag uploads a *candidate* to
Hackage; a maintainer promotes it by hand.

## Procedure

1. **Release-prep PR.** On a branch, stamp the date on the pending
   CHANGELOG entry (`## 0.2.0.2 - 2026-09-10`) and update the "Current
   state" paragraph in `specs/ROADMAP.md`. The version in
   `mcp-server.cabal` should already be right; check it follows the PVP
   comment block there. Merge the PR.

2. **Tag.** On the merged `main`:

   ```bash
   git tag v0.2.0.2
   git push origin v0.2.0.2
   ```

   The tag must be `v` followed by the exact cabal version.

3. **Wait for the `Hackage candidate` workflow.** It refuses to proceed if
   the tag and cabal version disagree, or if the CHANGELOG entry for that
   version is still `???`. Otherwise it runs `cabal check`, builds and
   tests the source distribution in isolation, builds the documentation
   tarball with `--haddock-for-hackage`, and uploads both as a candidate.

4. **Review and publish.** Open

   ```
   https://hackage.haskell.org/package/mcp-server-<version>/candidate
   ```

   check the rendered README, the module list and that the documentation
   is present, then use *Publish candidate*. Only this step makes the
   version public. Hackage versions cannot be deleted, only deprecated,
   so this is the moment to be sure.

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
cabal upload --token "$HACKAGE_AUTH_TOKEN" dist-release/mcp-server-<version>.tar.gz
cabal upload --token "$HACKAGE_AUTH_TOKEN" --documentation dist-newstyle/mcp-server-<version>-docs.tar.gz
```

Add `--publish` to both `upload` commands to skip the candidate stage.
