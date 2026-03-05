{
  description = "aihc development flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
    in {
      apps = forAllSystems (pkgs:
        let
          mkApp = name: text: {
            type = "app";
            program = "${pkgs.writeShellApplication {
              inherit name;
              runtimeInputs = [ pkgs.bash pkgs.cabal-install pkgs.ghc ];
              inherit text;
            }}/bin/${name}";
          };
        in {
          parser-test = mkApp "parser-test" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            cabal test --test-show-details=direct
          '';

          parser-progress = mkApp "parser-progress" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            cabal run h2010-progress
          '';

          parser-progress-strict = mkApp "parser-progress-strict" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            cabal run h2010-progress -- --strict
          '';

          name-resolution-test = mkApp "name-resolution-test" ''
            set -euo pipefail
            test -d components/haskell-name-resolution || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-name-resolution
            cabal test --test-show-details=direct
          '';

          name-resolution-progress = mkApp "name-resolution-progress" ''
            set -euo pipefail
            test -d components/haskell-name-resolution || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-name-resolution
            cabal run name-resolution-progress
          '';

          name-resolution-progress-strict = mkApp "name-resolution-progress-strict" ''
            set -euo pipefail
            test -d components/haskell-name-resolution || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-name-resolution
            cabal run name-resolution-progress -- --strict
          '';

          default = mkApp "default" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            cabal test --test-show-details=direct
          '';
        });
    };
}
