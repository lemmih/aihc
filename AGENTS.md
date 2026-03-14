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

- **Common status model**
  - Outcomes: `PASS`, `XFAIL`, `FAIL`, `XPASS`.
  - Any `FAIL` or unexpected `XPASS` should block merge until handled.

- **`haskell-parser`**
  - Golden regression tests:
    - Parser fixtures: `components/haskell-parser/test/Test/Fixtures/golden/`.
    - Lexer fixtures: `components/haskell-parser/test/Test/Fixtures/lexer/`.
    - Input/output snapshots with `pass`/`xfail`/`xpass` coverage.
  - Oracle compliance tests:
    - Haskell2010 manifest: `components/haskell-parser/test/Test/Fixtures/haskell2010/manifest.tsv`.
    - Extension manifests: per-extension `manifest.tsv` files under `components/haskell-parser/test/Test/Fixtures/`.
    - Oracle is GHC, with parser round-trip/fingerprint validation.
  - Fuzz completeness tests:
    - Run `nix run .#parser-fuzz -- ...`.
    - Generates random modules, finds parser-validation failures, and shrinks to minimal repros.
    - Promote minimized repros into golden/oracle fixtures.

- **`haskell-cpp`**
  - Oracle compliance tests:
    - Manifest: `components/haskell-cpp/test/Test/Fixtures/progress/manifest.tsv`.
    - Oracle is `cpphs`; outputs are compared against `cpphs` behavior.

- **`haskell-name-resolution`**
  - Fixture-driven progress and resolver tests under `components/haskell-name-resolution/test/Test/Fixtures/`.
  - Use `nix run .#name-resolution-progress` to track coverage progress.

## Pre-PR Review

- Run `coderabbit review --prompt-only` after local checks pass (including `nix flake check`) and before `gh pr create`.
- If CodeRabbit is offline or rate-limited, skip the review and open the PR.
- Resolve findings or explicitly justify remaining findings in the PR description.

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
