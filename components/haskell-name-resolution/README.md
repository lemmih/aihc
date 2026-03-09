# Haskell Name Resolution

This component resolves names over the parser AST and reports diagnostics such as unbound variables and duplicate top-level bindings.

## Progress Tracking

Coverage is tracked with a manifest-driven capability corpus:
- `test/Test/Fixtures/progress/manifest.tsv`

Each case is marked as:
- `pass`: expected to match oracle location-aware resolution facts (binding/use sites + diagnostics)
- `xfail`: known unsupported behavior (tracked gap)

Runtime outcomes:
- `PASS`: expected `pass`, facts match oracle
- `XFAIL`: expected `xfail`, still unresolved
- `XPASS`: expected `xfail`, now matches oracle (ready to promote in manifest)
- `FAIL`: regression (expected pass not matching) or oracle/fixture failure

Current baseline:
<!-- AUTO-GENERATED: START name-resolution-progress -->
- `10/12` implemented (`83.33%` complete)
<!-- AUTO-GENERATED: END name-resolution-progress -->

## Commands

Run all name-resolution tests:

```bash
nix run .#name-resolution-test
```

Run progress summary:

```bash
nix run .#name-resolution-progress
```

Strict mode (non-zero on `FAIL` or `XPASS`):

```bash
nix run .#name-resolution-progress-strict
```
