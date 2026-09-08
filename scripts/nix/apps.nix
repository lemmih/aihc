{
  projectHsPackages,
  mkHsPkgs,
}: pkgs: let
  hsPkgs = mkHsPkgs pkgs;
  resolveProgressExe = pkgs.lib.getExe' hsPkgs.aihc-resolve-tooling-common "resolve-progress";
  resolveExtensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-resolve-tooling-common "resolve-extension-progress";
  tcProgressExe = pkgs.lib.getExe' hsPkgs.aihc-tc "tc-progress";
  aihcDevExe = pkgs.lib.getExe' hsPkgs.aihc "aihc-dev";
  aihcExe = pkgs.lib.getExe' hsPkgs.aihc "aihc";
  unicode = import ./unicode.nix {inherit pkgs;};
  repoRootGuard = ''
    test -f cabal.project || {
      echo "Run this app from the repository root." >&2
      exit 1
    }
  '';

  mkAppWithInputs = name: runtimeInputs: text: {
    type = "app";
    program = "${pkgs.writeShellApplication {
      inherit name runtimeInputs text;
    }}/bin/${name}";
    meta.description = "aihc app: ${name}";
  };

  mkApp = name: text: mkAppWithInputs name [pkgs.bash pkgs.cabal-install hsPkgs.ghc] text;

  mkFmtApp = name: text:
    mkAppWithInputs name [pkgs.bash pkgs.git pkgs.findutils pkgs.alejandra (projectHsPackages pkgs).ormolu] text;

  mkReportsApp = name: text: {
    type = "app";
    program = "${pkgs.writeShellApplication {
      inherit name text;
      runtimeInputs = [pkgs.bash pkgs.nix];
    }}/bin/${name}";
    meta.description = "aihc app: ${name}";
  };

  mkComponentApp = name: component: text:
    mkApp name ''
      set -euo pipefail
      ${repoRootGuard}
      cd ${component}
      ${text}
    '';

  aihcApp = mkAppWithInputs "aihc" [pkgs.bash] ''
    exec ${aihcExe} +RTS -M2G -RTS "$@"
  '';
in {
  fmt = mkFmtApp "fmt" ''
    set -euo pipefail

    repo_root="$(git rev-parse --show-toplevel 2>/dev/null)" || {
      echo "Run this app from inside the repository." >&2
      exit 1
    }
    cd "$repo_root"

    git ls-files -z -- '*.nix' | xargs -0 -r alejandra
    git ls-files -z -- '*.hs' | grep -vz '/Fixtures/' | xargs -0 -r ormolu -m inplace
  '';

  generate-unicode = mkAppWithInputs "generate-unicode" [pkgs.bash pkgs.git pkgs.ormolu] ''
    set -euo pipefail
    ${repoRootGuard}

    repo_root="$(git rev-parse --show-toplevel)"
    cd "$repo_root"
    output="''${AIHC_UNICODE_OUTPUT:-$repo_root/core-libs/aihc-prim/src/GHC/Prim/Unicode.hs}"
    generated_dir="$(mktemp -d)"
    trap 'rm -rf "$generated_dir"' EXIT

    UNICODE_VERSION=${unicode.version} ${unicode.generator} \
      --input=${unicode.ucd}/ \
      --output="$generated_dir" \
      --core-prop=Uppercase \
      --core-prop=Lowercase
    cp "$generated_dir/GHC/Prim/Unicode.hs" "$output"
    ormolu --mode inplace "$output"
    echo "Generated $output from Unicode ${unicode.version}."
  '';

  check-unicode = mkAppWithInputs "check-unicode" [pkgs.bash pkgs.coreutils pkgs.diffutils pkgs.git pkgs.ormolu] ''
    set -euo pipefail
    ${repoRootGuard}

    repo_root="$(git rev-parse --show-toplevel)"
    cd "$repo_root"
    committed="$repo_root/core-libs/aihc-prim/src/GHC/Prim/Unicode.hs"
    generated_dir="$(mktemp -d)"
    generated="$generated_dir/GHC/Prim/Unicode.hs"
    trap 'rm -rf "$generated_dir"' EXIT

    UNICODE_VERSION=${unicode.version} ${unicode.generator} \
      --input=${unicode.ucd}/ \
      --output="$generated_dir" \
      --core-prop=Uppercase \
      --core-prop=Lowercase
    ormolu --mode inplace "$generated"

    if ! cmp --silent "$committed" "$generated"; then
      echo "Committed Unicode tables are stale. Run: just generate-unicode" >&2
      diff --unified "$committed" "$generated" || true
      exit 1
    fi
    echo "Committed Unicode tables match Unicode ${unicode.version}."
  '';

  line-counts = mkAppWithInputs "line-counts" [pkgs.tokei pkgs.jq pkgs.jtbl pkgs.bash] ''
    set -euo pipefail

    total_code=0
    total_tests=0

    {
      for comp_path in components/*; do
        [ -d "$comp_path" ] || continue
        comp=$(basename "$comp_path")

        # Skip aihc-name-resolution (empty stub).
        [ "$comp" = "aihc-name-resolution" ] && continue

        comp_all_lines=$(tokei "$comp_path" --output json | jq '.Total.code // 0')
        test_lines=0
        if [ -d "$comp_path/test" ]; then
          test_lines=$(tokei "$comp_path/test" --output json | jq '.Total.code // 0')
        fi
        # Apps are testing tools (fuzz, progress reports, etc.), count as test code.
        if [ -d "$comp_path/app" ]; then
          app_lines=$(tokei "$comp_path/app" --output json | jq '.Total.code // 0')
          test_lines=$((test_lines + app_lines))
        fi
        # Common contains shared test infrastructure (golden, Hedgehog, oracle, etc.).
        if [ -d "$comp_path/common" ]; then
          common_lines=$(tokei "$comp_path/common" --output json | jq '.Total.code // 0')
          test_lines=$((test_lines + common_lines))
        fi
        code_lines=$((comp_all_lines - test_lines))
        if [ "$code_lines" -lt 0 ]; then code_lines=0; fi

        comp_total=$comp_all_lines
        jq -nc \
          --arg Component "$comp" \
          --argjson Code "$code_lines" \
          --argjson Tests "$test_lines" \
          --argjson Total "$comp_total" \
          '{Component: $Component, Code: $Code, Tests: $Tests, Total: $Total}'

        total_code=$((total_code + code_lines))
        total_tests=$((total_tests + test_lines))
      done

      total_all=$((total_code + total_tests))
      jq -nc \
        --argjson Code "$total_code" \
        --argjson Tests "$total_tests" \
        --argjson Total "$total_all" \
        '{Component: "**Total**", Code: $Code, Tests: $Tests, Total: $Total}'
    } | {
      printf '%s\n' '```'
      jtbl --markdown
      printf '%s\n' '```'
    }
  '';

  aihc-dev = mkAppWithInputs "aihc-dev" [pkgs.bash hsPkgs.ghc] ''
    exec ${aihcDevExe} "$@"
  '';

  aihc = aihcApp;

  resolve-progress = mkComponentApp "resolve-progress" "components/aihc-resolve" ''
    ${resolveProgressExe} "$@"
  '';

  resolve-progress-strict = mkComponentApp "resolve-progress-strict" "components/aihc-resolve" ''
    ${resolveProgressExe} --strict "$@"
  '';

  resolve-extension-progress = mkComponentApp "resolve-extension-progress" "components/aihc-resolve" ''
    ${resolveExtensionProgressExe} "$@"
  '';

  tc-progress = mkComponentApp "tc-progress" "components/aihc-tc" ''
    ${tcProgressExe} "$@"
  '';

  tc-progress-strict = mkComponentApp "tc-progress-strict" "components/aihc-tc" ''
    ${tcProgressExe} --strict "$@"
  '';

  tc-test = mkComponentApp "tc-test" "components/aihc-tc" ''
    cabal test --test-show-details=direct
  '';

  generate-reports = mkReportsApp "generate-reports" ''
    set -euo pipefail
    ${repoRootGuard}
    bash ./scripts/update-generated-content.sh --update
  '';

  check-reports = mkReportsApp "check-reports" ''
    set -euo pipefail
    ${repoRootGuard}
    bash ./scripts/update-generated-content.sh --check
  '';

  default = aihcApp;
}
