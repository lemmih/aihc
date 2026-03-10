# AGENTS

- Tools: `gh`, `nix` (others via `nix`)
- Run tests: `nix flake check`
- Keep README AUTO-GENERATED blocks up to date before opening PRs
- Commands:
  - Parser: `nix run .#parser-progress`
  - Extensions: `nix run .#parser-extension-progress` (regenerate docs: `nix run .#parser-extension-progress -- --markdown | sed -n '/^# Haskell Parser Extension Support Status/,$p' > docs/haskell-parser-extension-support.md`)
  - CPP: `nix run .#cpp-progress`
  - Name-resolution: `nix run .#name-resolution-progress`

- Create PRs: `gh pr create --base main --head <branch> --title "<title>" --body <file>`
