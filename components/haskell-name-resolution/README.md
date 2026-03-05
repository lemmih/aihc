# Haskell Name Resolution

This component resolves names over the parser AST and reports diagnostics such as unbound variables and duplicate top-level bindings.

## Progress Tracking

Coverage is tracked with a manifest-driven capability corpus:
- `test/Test/Fixtures/progress/manifest.tsv`

Each case is marked as:
- `pass`: expected to match oracle resolution facts
- `xfail`: known unsupported behavior (tracked gap)

Runtime outcomes:
- `PASS`: expected `pass`, facts match oracle
- `XFAIL`: expected `xfail`, still unresolved
- `XPASS`: expected `xfail`, now matches oracle (ready to promote in manifest)
- `FAIL`: regression (expected pass not matching) or oracle/fixture failure

Current baseline:
- `8/12` implemented (`66.66%` complete)
- `PASS=8`, `XFAIL=4`, `XPASS=0`, `FAIL=0`

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
