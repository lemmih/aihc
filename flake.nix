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
        mkHsPkgs = pkgs:
          pkgs.haskellPackages.override {
             overrides = final: prev: {
               ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
               aihc-parser = final.callCabal2nix "aihc-parser" ./components/haskell-parser { };
               aihc-cpp = final.callCabal2nix "aihc-cpp" ./components/haskell-cpp { };
               aihc-name-resolution =
                 final.callCabal2nix "aihc-name-resolution" ./components/haskell-name-resolution { };
             };
           };
     in {
      apps = forAllSystems (pkgs:
        let
          hsPkgs = mkHsPkgs pkgs;
          parserProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "parser-progress";
          lexerProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "lexer-progress";
          extensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "extension-progress";
          parserFuzzExe = pkgs.lib.getExe' hsPkgs.aihc-parser "parser-fuzz";
          cppProgressExe = pkgs.lib.getExe' hsPkgs.aihc-cpp "cpp-progress";
          hackageTesterExe = pkgs.lib.getExe' hsPkgs.aihc-parser "hackage-tester";
          stackageProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "stackage-progress";
          nameResolutionProgressExe =
            pkgs.lib.getExe' hsPkgs.aihc-name-resolution "name-resolution-progress";
          mkAppWithInputs = name: runtimeInputs: text: {
            type = "app";
            program = "${pkgs.writeShellApplication {
              inherit name;
              inherit runtimeInputs;
              inherit text;
            }}/bin/${name}";
            meta.description = "aihc app: ${name}";
          };
          mkApp = name: text: mkAppWithInputs name [ pkgs.bash pkgs.cabal-install pkgs.ghc ] text;
          mkReportsApp = name: text: {
            type = "app";
            program = "${pkgs.writeShellApplication {
              inherit name;
              runtimeInputs = [ pkgs.bash pkgs.nix ];
              inherit text;
            }}/bin/${name}";
            meta.description = "aihc app: ${name}";
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
            ${parserProgressExe}
          '';

          lexer-progress = mkApp "lexer-progress" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            ${lexerProgressExe}
          '';

          parser-extension-progress = mkApp "parser-extension-progress" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            ${extensionProgressExe} "$@"
          '';

          parser-fuzz = mkApp "parser-fuzz" ''
            set -euo pipefail
            ${parserFuzzExe} "$@"
          '';

          hackage-tester = mkApp "hackage-tester" ''
            set -euo pipefail
            ${hackageTesterExe} "$@"
          '';

          stackage-progress = mkApp "stackage-progress" ''
            set -euo pipefail
            ${stackageProgressExe} "$@"
          '';

          parser-progress-strict = mkApp "parser-progress-strict" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            ${parserProgressExe} --strict
          '';

          lexer-progress-strict = mkApp "lexer-progress-strict" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            ${lexerProgressExe} --strict
          '';

          parser-extension-progress-strict = mkApp "parser-extension-progress-strict" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            ${extensionProgressExe} --strict "$@"
          '';

          cpp-test = mkApp "cpp-test" ''
            set -euo pipefail
            test -d components/haskell-cpp || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-cpp
            cabal test --test-show-details=direct
          '';

          cpp-progress = mkApp "cpp-progress" ''
            set -euo pipefail
            test -d components/haskell-cpp || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-cpp
            ${cppProgressExe} "$@"
          '';

          cpp-progress-strict = mkApp "cpp-progress-strict" ''
            set -euo pipefail
            test -d components/haskell-cpp || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-cpp
            ${cppProgressExe} --strict "$@"
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
            ${nameResolutionProgressExe}
          '';

          name-resolution-progress-strict = mkApp "name-resolution-progress-strict" ''
            set -euo pipefail
            test -d components/haskell-name-resolution || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-name-resolution
            ${nameResolutionProgressExe} --strict
          '';

          generate-reports = mkReportsApp "generate-reports" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            bash ./scripts/update-generated-content.sh --update
          '';

          check-reports = mkReportsApp "check-reports" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            bash ./scripts/update-generated-content.sh --check
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
          hsPkgs = mkHsPkgs pkgs;
          parserTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgs.aihc-parser);
          cppTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgs.aihc-cpp);
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
          parserProgressStrict = pkgs.runCommand "aihc-parser-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-parser ];
          } ''
            cd "$src/components/haskell-parser"
            parser-progress --strict
            touch "$out"
          '';
          lexerProgressStrict = pkgs.runCommand "aihc-lexer-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-parser ];
          } ''
            cd "$src/components/haskell-parser"
            lexer-progress --strict
            touch "$out"
          '';
          parserExtensionProgressStrict = pkgs.runCommand "aihc-parser-extension-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-parser ];
          } ''
            cd "$src/components/haskell-parser"
            extension-progress --strict
            touch "$out"
          '';
          cppProgressStrict = pkgs.runCommand "aihc-cpp-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-cpp ];
          } ''
            cd "$src/components/haskell-cpp"
            cpp-progress --strict
            touch "$out"
          '';
          nameResolutionProgressStrict = pkgs.runCommand "aihc-name-resolution-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-name-resolution ];
          } ''
            cd "$src/components/haskell-name-resolution"
            name-resolution-progress --strict
            touch "$out"
          '';
        in {
          parser-tests = parserTests;
          cpp-tests = cppTests;
          name-resolution-tests = nameResolutionTests;
          parser-progress-strict = parserProgressStrict;
          lexer-progress-strict = lexerProgressStrict;
          parser-extension-progress-strict = parserExtensionProgressStrict;
          cpp-progress-strict = cppProgressStrict;
          name-resolution-progress-strict = nameResolutionProgressStrict;
           nix-lint = nixLint;
           haskell-lint = haskellLint;
           haskell-format = haskellFormat;
           all-tests =
              pkgs.linkFarm "aihc-all-tests" [
                { name = "parser-tests"; path = parserTests; }
                { name = "cpp-tests"; path = cppTests; }
                { name = "name-resolution-tests"; path = nameResolutionTests; }
                { name = "parser-progress-strict"; path = parserProgressStrict; }
                { name = "lexer-progress-strict"; path = lexerProgressStrict; }
                { name = "parser-extension-progress-strict"; path = parserExtensionProgressStrict; }
                { name = "cpp-progress-strict"; path = cppProgressStrict; }
                { name = "name-resolution-progress-strict"; path = nameResolutionProgressStrict; }
                 { name = "nix-lint"; path = nixLint; }
                { name = "haskell-lint"; path = haskellLint; }
                { name = "haskell-format"; path = haskellFormat; }
             ];
        });
    };
}
