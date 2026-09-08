# Test runner for aihc project
# See https://just.systems for Just documentation

# Run all tests with hidden successes (1000 Hedgehog tests per property)
test:
  cabal test -v0 all --test-options='--hide-successes --hedgehog-tests 1000 --hedgehog-shrinks 10000'

# Replay a specific Hedgehog test case
# Usage: just replay "<replay-string>"
replay ARGUMENT:
  cabal test all --jobs=1 -v0 --test-options='--pattern properties --hedgehog-replay="{{ARGUMENT}}" --hide-successes'

# Run Hedgehog with 10000 tests in a loop until failure
hedgehog:
  while true; do just hedgehog1 || break; done

hedgehog1:
  cabal test all -v0 --jobs=1 --test-options="--hedgehog-tests 10000 --hedgehog-shrinks 1000000 --hide-successes"

# Auto-format Nix, Cabal, Haskell, and C files (excludes dist-newstyle, result, .git; Haskell excludes test fixtures)
fmt:
  nix develop --quiet --command bash -c 'find . -name "*.nix" -not -path "*/.git/*" -not -path "*/dist-newstyle/*" -not -path "*/result/*" -print0 | xargs -0 -r alejandra; while IFS= read -r -d "" file; do cabal-gild --mode format --io "$file"; done < <(find . -name "*.cabal" -not -path "*/.git/*" -not -path "*/dist-newstyle/*" -not -path "*/result/*" -print0); ormolu --mode inplace $(find components tooling bin core-libs scripts/nix/ucd2haskell-aihc -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*"); find components tooling bin core-libs scripts test -type f \( -name "*.c" -o -name "*.h" \) -not -path "*/dist-newstyle/*" -print0 | xargs -0 -r clang-format -i'

# Apply HLint hints in place via apply-refact (HLint --refactor accepts one file at a time; same file set as fmt/check)
hlint-refactor:
  #!/usr/bin/env bash
  set -euo pipefail
  exec nix develop --quiet --command bash -c \
    'set -euo pipefail
     while IFS= read -r -d "" f; do
       hlint --refactor --refactor-options="--inplace" "$f"
     done < <(find components tooling bin core-libs scripts/nix/ucd2haskell-aihc -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*" -print0)'

# Run full CI check: format, lint, then tests (warnings are errors only here, not in plain `cabal` / `just test`)
check:
  nix build .#user-guide --no-link
  nix develop --quiet --command bash -c 'failed=0; while IFS= read -r -d "" file; do cabal-gild --mode check --input "$file" || failed=1; done < <(find . -name "*.cabal" -not -path "*/.git/*" -not -path "*/dist-newstyle/*" -not -path "*/result/*" -print0); exit "$failed"'
  nix develop --quiet --command bash -c 'ormolu --mode check $(find components tooling bin core-libs scripts/nix/ucd2haskell-aihc -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*")'
  nix develop --quiet --command bash -c 'hlint -j $(find components tooling bin core-libs scripts/nix/ucd2haskell-aihc -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*")'
  nix develop --quiet --command bash -c 'find components tooling bin core-libs scripts test -type f \( -name "*.c" -o -name "*.h" \) -not -path "*/dist-newstyle/*" -print0 | xargs -0 -r clang-format --dry-run --Werror'
  nix develop --quiet --command bash -c 'set -euo pipefail; bindings_directory=$(mktemp -d); trap '\''rm -rf "$bindings_directory"'\'' EXIT; wit-bindgen c --world command --out-dir "$bindings_directory" bin/aihc/compiler/wasm/runtime/wit; while IFS= read -r -d "" file; do if [[ "$file" == *bin/aihc/compiler/wasm/runtime/*.c || "$file" == *aihc_host_wasip3.c ]]; then clang-tidy-unwrapped --quiet "$file" -- --target=wasm32-wasip1 --sysroot="$AIHC_WASM_SYSROOT" -std=c11 -Wall -Wextra -Wpedantic -Ibin/aihc/compiler/wasm/runtime -Ibin/aihc/compiler/native/runtime -isystem "$bindings_directory"; else clang-tidy --quiet "$file" -- -std=c11 -Wall -Wextra -Wpedantic; fi; done < <(find components tooling bin core-libs scripts test -type f -name "*.c" -not -path "*/dist-newstyle/*" -print0)'
  TASTY_HEDGEHOG_TESTS=1000 cabal test -v0 all --ghc-options=-Werror --test-options='--hide-successes'

# Preview the user guide at http://127.0.0.1:8000/.
docs:
  nix develop --quiet --command mkdocs serve --config-file docs/aihc-users-guide/mkdocs.yml

# Generate boot package interfaces for the resolver (requires GHC dev env)
gen-boot-ifaces:
  #!/usr/bin/env bash
  set -euo pipefail
  GHC_VERSION=$(ghc --numeric-version)
  CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/aihc-resolve/boot-interfaces/$GHC_VERSION"
  mkdir -p "$CACHE_DIR"
  for pkg in ghc-prim ghc-internal ghc-bignum base; do
    echo "Extracting interface for $pkg..."
    cabal run -v0 aihc-dev -- extract-resolve-iface --package "$pkg" --output "$CACHE_DIR/$pkg.json"
  done
  echo "Boot interfaces generated in $CACHE_DIR"

# Regenerate the committed Unicode tables from pinned UCD inputs.
generate-unicode:
  nix run .#generate-unicode

# Verify that the committed Unicode tables match the generator and pinned UCD.
check-unicode:
  nix run .#check-unicode
