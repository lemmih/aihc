# AI-written Haskell Compiler (aihc)

## Docs

- [Haskell 2010 Language Report (Markdown)](docs/haskell2010-language-report.md)
- [Compiler Pipeline Overview](PIPELINE.md)

## Parser Progress

The from-scratch parser lives in `components/haskell-parser`.

Current Haskell2010 progress:
<!-- AUTO-GENERATED: START parser-progress -->
- `234/239` syntax cases implemented (`97.90%` complete)
<!-- AUTO-GENERATED: END parser-progress -->

## Haskell Parser Extension Support Progress

Extension support tracking for `components/haskell-parser` is reported in:
- [Haskell Parser Extension Support Status](docs/haskell-parser-extension-support.md)

Current extension baseline:
<!-- AUTO-GENERATED: START parser-extension-progress -->
- Total tracked extensions: `58`
- Supported: `4`
- In Progress: `1`
- Planned: `53`
- Supported extensions: `ParallelListComp, QuasiQuotes, TypeApplications, ViewPatterns`
- In Progress extensions: `LambdaCase`
<!-- AUTO-GENERATED: END parser-extension-progress -->

## CPP Preprocessor Progress

The pure CPP component lives in `components/haskell-cpp`.

Current progress:
<!-- AUTO-GENERATED: START cpp-progress -->
- `9/14` preprocessing cases implemented (`64.28%` complete)
<!-- AUTO-GENERATED: END cpp-progress -->

## Name-Resolution Progress

The name-resolution component lives in `components/haskell-name-resolution`.

Current progress:
<!-- AUTO-GENERATED: START name-resolution-progress -->
- `10/12` capability cases implemented (`83.33%` complete)
<!-- AUTO-GENERATED: END name-resolution-progress -->

Generate all tracked docs/sections:

```bash
nix run .#generate-reports
```

Check generated content is up to date:

```bash
nix run .#check-reports
```
