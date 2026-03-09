# aihc

## Docs

- [Haskell 2010 Language Report (Markdown)](docs/haskell2010-language-report.md)
- [Compiler Pipeline Overview](PIPELINE.md)

## Parser Progress

The from-scratch parser lives in `components/haskell-parser`.

Current Haskell2010 progress:
<!-- AUTO-GENERATED: START parser-progress -->
- `230/230` syntax cases implemented (`100.00%` complete)
- status breakdown: `PASS=230`, `XFAIL=0`, `XPASS=0`, `FAIL=0`
<!-- AUTO-GENERATED: END parser-progress -->

Recompute progress with:

```bash
nix run .#parser-progress
```

## Haskell Parser Extension Support Progress

Extension support tracking for `components/haskell-parser` is reported in:
- [Haskell Parser Extension Support Status](docs/haskell-parser-extension-support.md)

Current extension baseline:
<!-- AUTO-GENERATED: START parser-extension-progress -->
- Total tracked extensions: `33`
- Supported: `4`
- In Progress: `0`
- Planned: `29`
<!-- AUTO-GENERATED: END parser-extension-progress -->

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

## CPP Preprocessor Progress

The pure CPP component lives in `components/haskell-cpp`.

Current progress:
<!-- AUTO-GENERATED: START cpp-progress -->
- `9/14` preprocessing cases implemented (`64.28%` complete)
- status breakdown: `PASS=9`, `XFAIL=5`, `XPASS=0`, `FAIL=0`
<!-- AUTO-GENERATED: END cpp-progress -->

Recompute progress with:

```bash
nix run .#cpp-progress
```

## Name-Resolution Progress

The name-resolution component lives in `components/haskell-name-resolution`.

Current progress:
<!-- AUTO-GENERATED: START name-resolution-progress -->
- `10/12` capability cases implemented (`83.33%` complete)
- status breakdown: `PASS=10`, `XFAIL=2`, `XPASS=0`, `FAIL=0`
<!-- AUTO-GENERATED: END name-resolution-progress -->

Recompute progress with:

```bash
nix run .#name-resolution-progress
```

Generate all tracked docs/sections:

```bash
nix run .#generate-reports
```

Check generated content is up to date:

```bash
nix run .#check-reports
```
