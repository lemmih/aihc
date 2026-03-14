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
