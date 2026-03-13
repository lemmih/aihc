# Haskell Parser

This component contains a from-scratch Haskell parser plus oracle-backed tests.

## Haskell2010 Coverage Tracking

The test suite includes a manifest-driven Haskell2010 syntax corpus under:
- `test/Test/Fixtures/haskell2010/manifest.tsv`

Each case is marked with one expected status:
- `pass`: parser is expected to accept the case
- `xfail`: known unimplemented syntax; parser is expected to reject it for now

Runtime outcomes are reported as:
- `PASS`: expected `pass`, oracle accepts source, and `parse -> pretty` preserves GHC AST
- `XFAIL`: expected `xfail`, case still fails oracle and/or round-trip checks
- `XPASS`: expected `xfail`, case now passes both oracle and round-trip checks
- `FAIL`: regression or invalid case/manifest (for example oracle rejects a `pass` case)

Current progress baseline:
<!-- AUTO-GENERATED: START haskell2010-progress -->
- `212/361` implemented (`58.72%` complete)
<!-- AUTO-GENERATED: END haskell2010-progress -->

## Extension Coverage Tracking

Tracked extensions are listed in:
- `test/Test/Fixtures/extensions.tsv`

Each extension can provide a manifest at:
- `test/Test/Fixtures/<Extension>/manifest.tsv`

Current extension baseline:
<!-- AUTO-GENERATED: START extension-progress -->
- Total tracked extensions: `60`
- Supported: `10`
- In Progress: `21`
- Planned: `29`
<!-- AUTO-GENERATED: END extension-progress -->

Generated report:
- `../../docs/haskell-parser-extension-support.md`

## Commands

Run full tests:

```bash
nix run .#parser-test
```

Run progress summary:

```bash
nix run .#parser-progress
```

Run extension support summary:

```bash
nix run .#parser-extension-progress
```

Strict mode (non-zero exit on regressions or `XPASS`):

```bash
nix run .#parser-progress-strict
```

Extension strict mode (non-zero exit on regressions or `XPASS`):

```bash
nix run .#parser-extension-progress-strict
```

## Extension Support

Beyond Haskell2010, we track support for various Haskell language extensions. Each extension has its own test directory under:
- `test/Test/Fixtures/<ExtensionName>/manifest.tsv`

### Generating Extension Status Report

```bash
nix run .#parser-extension-progress -- --markdown \
  | sed -n '/^# Haskell Parser Extension Support Status/,$p'
```

This generates a markdown report showing:
- Total extensions tracked
- Supported extensions (all tests passing)
- Partial support (some tests passing)
- Haskell2010 baseline

### Adding a New Extension

1. Create a new directory: `test/Test/Fixtures/<ExtensionName>/`
2. Add test files (valid Haskell source with the extension enabled)
3. Create `manifest.tsv` with test cases in the same format as Haskell2010

The manifest format:
```
<test-id>	<category>	<path/to/file.hs>	<pass|xfail>	<reason>
```

Example:
```
list-comp-parallel-1	expressions	list-comp.hs	pass	parallel list comprehension
```

### NIX Commands

- `nix run .#parser-extension-progress -- --markdown | sed -n '/^# Haskell Parser Extension Support Status/,$p'` - Generate clean markdown report to stdout
- `nix build .#extension-report` - Build report to result/ directory
- `nix flake check` - Includes extension report as part of CI checks

## Hackage Testing

To validate the parser against real-world Haskell packages from Hackage:

```bash
nix run .#hackage-tester -- <package-name>
```

Example:
```bash
nix run .#hackage-tester -- transformers
```

The tool:
- Downloads and caches packages locally in `~/.cache/aihc/hackage/`
- Runs the in-repo CPP preprocessor before parsing (with best-effort include resolution)
- Parses Cabal-declared library and executable source files
- Reports parse errors and roundtrip failures
- Shows success rate for the package
