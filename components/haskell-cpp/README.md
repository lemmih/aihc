# Haskell CPP

This component implements a pure Haskell C preprocessor used by the parser pipeline.

## Why not use an off-the-shelf CPP?

- We do not want to depend on an external system `cpp` binary, because availability and behavior vary by environment/version.
- We do not want to depend on `cpphs` as a runtime library due to licensing constraints (LGPL) and because we want full control of the implementation roadmap.
- We still use `cpphs` in tests as an oracle to measure behavioral compatibility.

## Progress Tracking

Coverage is tracked with a manifest-driven corpus under:
- `test/Test/Fixtures/progress/manifest.tsv`

Current baseline:
<!-- AUTO-GENERATED: START cpp-progress -->
- `9/14` implemented (`64.28%` complete)
<!-- AUTO-GENERATED: END cpp-progress -->

## Commands

Run all cpp tests:

```bash
nix run .#cpp-test
```

Run progress summary:

```bash
nix run .#cpp-progress
```

Strict mode (non-zero on `FAIL` or `XPASS`):

```bash
nix run .#cpp-progress-strict
```
