# aihc

## Docs

- [Haskell 2010 Language Report (Markdown)](docs/haskell2010-language-report.md)
- [Compiler Pipeline Overview](PIPELINE.md)

## Parser Progress

The from-scratch parser lives in `components/haskell-parser`.

Current Haskell2010 progress:
- `119/194` syntax cases implemented (`61.34%` complete)
- status breakdown: `PASS=119`, `XFAIL=75`, `XPASS=0`, `FAIL=0`

Recompute progress with:

```bash
nix run .#parser-progress
```

Strict progress gate (fails on `FAIL` or `XPASS`):

```bash
nix run .#parser-progress-strict
```

## Name-Resolution Progress

The name-resolution component lives in `components/haskell-name-resolution`.

Current progress:
- `8/12` capability cases implemented (`66.66%` complete)
- status breakdown: `PASS=8`, `XFAIL=4`, `XPASS=0`, `FAIL=0`

Recompute progress with:

```bash
nix run .#name-resolution-progress
```

Strict progress gate (fails on `FAIL` or `XPASS`):

```bash
nix run .#name-resolution-progress-strict
```

## CI / Flake Checks

`nix flake check` includes strict progress checks for both components, so any `XPASS` now fails the check until the corresponding manifest rows are promoted from `xfail` to `pass`.
