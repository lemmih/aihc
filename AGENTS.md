# AGENTS

- Tools: `gh`, `nix` (others via `nix`)
- Run tests: `nix flake check`
- Include changes to progress counts in PR descriptions. Do not update the READMEs, though. THey are updated by a cron workflow.
- Commands:
  - Parser: `nix run .#parser-progress`
  - Extensions: `nix run .#parser-extension-progress`
  - CPP: `nix run .#cpp-progress`
  - Name-resolution: `nix run .#name-resolution-progress`

- Create PRs: `gh pr create --base main --head <branch> --title "<title>" --body <file>`
