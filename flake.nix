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

      checks = forAllSystems (pkgs:
        let
          hsPkgs = pkgs.haskellPackages.override {
            overrides = final: prev: {
              ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
              aihc-parser = final.callCabal2nix "aihc-parser" ./components/haskell-parser { };
              aihc-name-resolution =
                final.callCabal2nix "aihc-name-resolution" ./components/haskell-name-resolution { };
            };
          };
          parserTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgs.aihc-parser);
          nameResolutionTests =
            pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgs.aihc-name-resolution);
          nixLint = pkgs.runCommand "aihc-nix-lint" {
            src = ./.;
            nativeBuildInputs = [ pkgs.statix ];
          } ''
            cd "$src"
            statix check flake.nix
            touch "$out"
          '';
          haskellLint = pkgs.runCommand "aihc-haskell-lint" {
            src = ./.;
            nativeBuildInputs = [ pkgs.haskellPackages.hlint ];
          } ''
            cd "$src"
            find components -type f -name '*.hs' ! -path '*/test/Test/Fixtures/*' -print0 \
              | xargs -0 -r hlint
            touch "$out"
          '';
          haskellFormat = pkgs.runCommand "aihc-haskell-format" {
            src = ./.;
            nativeBuildInputs = [ pkgs.haskellPackages.ormolu ];
          } ''
            cd "$src"
            find components -type f -name '*.hs' ! -path '*/test/Test/Fixtures/*' -print0 \
              | xargs -0 -r ormolu --mode check
            touch "$out"
          '';
        in {
          parser-tests = parserTests;
          name-resolution-tests = nameResolutionTests;
          nix-lint = nixLint;
          haskell-lint = haskellLint;
          haskell-format = haskellFormat;
          all-tests =
            pkgs.linkFarm "aihc-all-tests" [
              { name = "parser-tests"; path = parserTests; }
              { name = "name-resolution-tests"; path = nameResolutionTests; }
              { name = "nix-lint"; path = nixLint; }
              { name = "haskell-lint"; path = haskellLint; }
              { name = "haskell-format"; path = haskellFormat; }
            ];
        });
    };
}
