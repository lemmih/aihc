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
- `4/44` implemented (`9.09%` complete)
- `PASS=4`, `XFAIL=40`, `XPASS=0`, `FAIL=0`

## Commands

Run full tests:

```bash
nix shell nixpkgs#ghc nixpkgs#cabal-install --command bash -lc 'cd components/haskell-parser && cabal test --test-show-details=direct'
```

Run progress summary:

```bash
nix shell nixpkgs#ghc nixpkgs#cabal-install --command bash -lc 'cd components/haskell-parser && cabal run h2010-progress'
```

Strict mode (non-zero exit on regressions or `XPASS`):

```bash
nix shell nixpkgs#ghc nixpkgs#cabal-install --command bash -lc 'cd components/haskell-parser && cabal run h2010-progress -- --strict'
```
