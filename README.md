# aihc

## Docs

- [Haskell 2010 Language Report (Markdown)](docs/haskell2010-language-report.md)
- [Compiler Pipeline Overview](PIPELINE.md)

## Parser Progress

The from-scratch parser lives in `components/haskell-parser`.

Current Haskell2010 progress:
- `213/213` syntax cases implemented (`100.00%` complete)
- status breakdown: `PASS=213`, `XFAIL=0`, `XPASS=0`, `FAIL=0`

Recompute progress with:

```bash
nix run .#parser-progress
```

## Haskell Parser Extension Support Progress

Extension support tracking for `components/haskell-parser` is reported in:
- [Haskell Parser Extension Support Status](docs/haskell-parser-extension-support.md)

Current extension baseline:
- Total tracked extensions: `33`
- Supported: `0`
- In Progress: `1`
- Planned: `32`

Recompute extension status with:

```bash
nix run .#parser-extension-progress
```

Regenerate the markdown report with:

```bash
nix run .#parser-extension-progress -- --markdown \
  | sed -n '/^# Haskell Parser Extension Support Status/,$p' \
  > docs/haskell-parser-extension-support.md
```

## Name-Resolution Progress

The name-resolution component lives in `components/haskell-name-resolution`.

Current progress:
- `10/12` capability cases implemented (`83.33%` complete)
- status breakdown: `PASS=10`, `XFAIL=2`, `XPASS=0`, `FAIL=0`

Recompute progress with:

```bash
nix run .#name-resolution-progress
```
