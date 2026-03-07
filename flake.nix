{
  description = "aihc development flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      lib = import ./nix/extensions.nix { inherit lib; };
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
      fixturesDir = ./. + "/components/haskell-parser/test/Test/Fixtures";
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

          parser-extension-progress = mkApp "parser-extension-progress" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            cabal run extension-progress -- "$@"
          '';

          hackage-tester = mkApp "hackage-tester" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            cabal run hackage-tester -- "$@"
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

          parser-extension-progress-strict = mkApp "parser-extension-progress-strict" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            cabal run extension-progress -- --strict "$@"
          '';

          extension-report = mkApp "extension-report" ''
            set -euo pipefail
            test -d components/haskell-parser || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/haskell-parser
            nix run --show-trace -f "${self}/default.nix" extension-report
          '';

          extension-report-markdown = mkApp "extension-report-markdown" ''
            set -euo pipefail
            fixtures="components/haskell-parser/test/Test/Fixtures"
            test -d "$fixtures" || {
              echo "Fixtures directory not found" >&2
              exit 1
            }
            
            extensions=$(
              find "$fixtures" -mindepth 1 -maxdepth 1 -type d ! -name 'haskell2010' -printf '%f\n' 2>/dev/null || true
            )
            
            haskell2010_manifest="$fixtures/haskell2010/manifest.tsv"
            h2010_pass=0
            h2010_total=0
            
            if [ -f "$haskell2010_manifest" ]; then
              while IFS=$'\t' read -r _ _ _ test_expected _; do
                [ -z "$test_expected" ] && continue
                h2010_total=$((h2010_total + 1))
                if [ "$test_expected" = "pass" ]; then
                  h2010_pass=$((h2010_pass + 1))
                fi
              done < <(grep -v '^#' "$haskell2010_manifest" | grep -v '^$')
            fi
            
            total_exts=0
            supported=0
            partial=0
            
            echo "# Haskell Extension Support Status"
            echo ""
            echo "**Generated**: $(date +%Y-%m-%d)"
            echo ""
            echo "## Summary"
            echo ""
            
            for ext in $extensions; do
              manifest="$fixtures/$ext/manifest.tsv"
              if [ -f "$manifest" ]; then
                ext_pass=0
                ext_total=0
                while IFS=$'\t' read -r _ _ _ test_expected _; do
                  [ -z "$test_expected" ] && continue
                  ext_total=$((ext_total + 1))
                  if [ "$test_expected" = "pass" ]; then
                    ext_pass=$((ext_pass + 1))
                  fi
                done < <(grep -v '^#' "$manifest" | grep -v '^$')
                
                total_exts=$((total_exts + 1))
                
                if [ "$ext_total" -gt 0 ]; then
                  if [ "$ext_pass" -eq "$ext_total" ]; then
                    supported=$((supported + 1))
                    status="supported"
                  elif [ "$ext_pass" -gt 0 ]; then
                    partial=$((partial + 1))
                    status="partial"
                  else
                    status="none"
                  fi
                  echo "| $ext | $status | $ext_pass/$ext_total | |"
                fi
              fi
            done
            
            echo "- Total Extensions: $total_exts"
            echo "- Supported: $supported"
            echo "- Partial: $partial"
            echo "- No Tests: $((total_exts - supported - partial))"
            echo ""
            echo "## Haskell2010 Support"
            echo ""
            echo "- Tests Passing: $h2010_pass/$h2010_total"
            echo ""
            echo "## Extension Status"
            echo ""
            echo "| Extension | Status | Tests | Notes |"
            echo "|-----------|--------|-------|-------|"
>>>>>>> 494bdb7 (feat: add hackage-tester tool for parsing real-world Haskell packages)
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
          extensionReport = pkgs.runCommand "aihc-extension-report" {
            src = ./.;
            fixturesDir = ./components/haskell-parser/test/Test/Fixtures;
          } ''
            mkdir -p "$out"
            fixtures="$src/components/haskell-parser/test/Test/Fixtures"
            
            extensions=$(
              find "$fixtures" -mindepth 1 -maxdepth 1 -type d ! -name 'haskell2010' -printf '%f\n' 2>/dev/null || true
            )
            
            haskell2010_manifest="$fixtures/haskell2010/manifest.tsv"
            h2010_pass=0
            h2010_total=0
            
            if [ -f "$haskell2010_manifest" ]; then
              while IFS=$'\t' read -r _ _ _ test_expected _; do
                [ -z "$test_expected" ] && continue
                h2010_total=$((h2010_total + 1))
                if [ "$test_expected" = "pass" ]; then
                  h2010_pass=$((h2010_pass + 1))
                fi
              done < <(grep -v '^#' "$haskell2010_manifest" | grep -v '^$')
            fi
            
            total_exts=0
            supported=0
            partial=0
            
            {
              echo "# Haskell Extension Support Status"
              echo ""
              echo "**Generated**: $(date +%Y-%m-%d)"
              echo ""
              echo "## Summary"
              echo ""
              
              for ext in $extensions; do
                manifest="$fixtures/$ext/manifest.tsv"
                if [ -f "$manifest" ]; then
                  ext_pass=0
                  ext_total=0
                  while IFS=$'\t' read -r _ _ _ test_expected _; do
                    [ -z "$test_expected" ] && continue
                    ext_total=$((ext_total + 1))
                    if [ "$test_expected" = "pass" ]; then
                      ext_pass=$((ext_pass + 1))
                    fi
                  done < <(grep -v '^#' "$manifest" | grep -v '^$')
                  
                  total_exts=$((total_exts + 1))
                  
                  if [ "$ext_total" -gt 0 ]; then
                    if [ "$ext_pass" -eq "$ext_total" ]; then
                      supported=$((supported + 1))
                      status="supported"
                    elif [ "$ext_pass" -gt 0 ]; then
                      partial=$((partial + 1))
                      status="partial"
                    else
                      status="none"
                    fi
                    echo "| $ext | $status | $ext_pass/$ext_total | |"
                  fi
                fi
              done
              
              echo "- Total Extensions: $total_exts"
              echo "- Supported: $supported"
              echo "- Partial: $partial"
              echo "- No Tests: $((total_exts - supported - partial))"
              echo ""
              echo "## Haskell2010 Support"
              echo ""
              echo "- Tests Passing: $h2010_pass/$h2010_total"
              echo ""
              echo "## Extension Status"
              echo ""
              echo "| Extension | Status | Tests | Notes |"
              echo "|-----------|--------|-------|-------|"
            } > "$out/extension-support.md"
            
            touch "$out"
          '';
          parserProgressStrict = pkgs.runCommand "aihc-parser-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-parser ];
          } ''
            cd "$src/components/haskell-parser"
            h2010-progress --strict
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
          name-resolution-tests = nameResolutionTests;
          parser-progress-strict = parserProgressStrict;
          parser-extension-progress-strict = parserExtensionProgressStrict;
          name-resolution-progress-strict = nameResolutionProgressStrict;
          nix-lint = nixLint;
          haskell-lint = haskellLint;
          haskell-format = haskellFormat;
          extension-report = extensionReport;
          all-tests =
            pkgs.linkFarm "aihc-all-tests" [
              { name = "parser-tests"; path = parserTests; }
              { name = "name-resolution-tests"; path = nameResolutionTests; }
              { name = "parser-progress-strict"; path = parserProgressStrict; }
              { name = "parser-extension-progress-strict"; path = parserExtensionProgressStrict; }
              { name = "name-resolution-progress-strict"; path = nameResolutionProgressStrict; }
              { name = "nix-lint"; path = nixLint; }
              { name = "haskell-lint"; path = haskellLint; }
              { name = "haskell-format"; path = haskellFormat; }
              { name = "extension-report"; path = extensionReport; }
            ];
        });
    };
}
