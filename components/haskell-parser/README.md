# Haskell Parser

This component contains a from-scratch Haskell parser plus oracle-backed tests.

## Haskell2010 Coverage Tracking

The test suite includes a manifest-driven Haskell2010 syntax corpus under:
- `test/Test/Fixtures/haskell2010/manifest.tsv`

Each case is marked with one expected status:
- `pass`: parser is expected to accept the case
- `xfail`: known unimplemented syntax; parser is expected to reject it for now

Runtime outcomes are reported as:
- `PASS`: expected `pass`, parser accepted, oracle accepted
- `XFAIL`: expected `xfail`, parser rejected, oracle accepted
- `XPASS`: expected `xfail`, parser accepted (feature implemented; manifest can be promoted)
- `FAIL`: regression or invalid case/manifest (for example oracle rejects a `pass` case)

Current progress baseline:
- `119/194` implemented (`61.34%` complete)
- `PASS=77`, `XFAIL=75`, `XPASS=42`, `FAIL=0`

## Commands

Run full tests:

```bash
nix run .#parser-test
```

Run progress summary:

```bash
nix run .#parser-progress
```

Strict mode (non-zero exit on regressions or `XPASS`):

```bash
nix run .#parser-progress-strict
```
