# AGENTS

- Tools: `gh`, `nix` (others via `nix`)
- Run tests: `nix flake check`
- Include changes to progress counts in PR descriptions. Do not update the READMEs, though. THey are updated by a cron workflow.
- Commands:
  - Parser: `nix run .#parser-progress`
  - Extensions: `nix run .#parser-extension-progress`
  - CPP: `nix run .#cpp-progress`
  - Name-resolution: `nix run .#name-resolution-progress`

- Create PRs: `gh pr create --base main --head <branch> --title "<title>" --body $(cat <file>)`
- PR titles should follow the same Conventional Commits format as commit messages (see below)

## Testing (TDD)

aihc is developed test-first. Run the full suite with `nix flake check`.

- **Golden tests (regressions)**  
  Fixtures under `test/Test/Fixtures/golden/` (parser expr/module) and lexer fixtures store input + expected output (AST or tokens) and status (`pass`/`fail`/`xpass`/`xfail`). Each run compares actual output to the golden snapshot; a summary test fails on any regression (`FAIL`) or unexpected pass (`XPASS`). Use to lock behavior and catch regressions.

- **Oracle tests (compliance)**  
  Haskell2010 and extension coverage are manifest-driven (e.g. `haskell2010/manifest.tsv`, per-extension manifests). GHC is the oracle: each case is parsed by aihc and by GHC; pass means aihc agrees (parse + round-trip AST fingerprint). Outcomes: `PASS`, `XFAIL` (known gap), `FAIL` (regression or oracle rejects a pass case), `XPASS` (xfail now passes). Use to measure and enforce compliance with GHC.

- **Fuzz tests (completeness)**  
  The parser-fuzz app generates random Haskell modules (HSE Arbitrary), runs them through the parser and validation (oracle round-trip). It searches for inputs that the oracle accepts but aihc rejects, then shrinks to a minimal repro. Use to find missing syntax/coverage; add new cases as golden or oracle fixtures and fix.

## Commits

Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>[optional scope]: <description>
```

**Types:** `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `build`, `ci`, `chore`

**Examples:**

```
feat: add user authentication
fix(auth): resolve null pointer in login
docs: update installation guide
```
