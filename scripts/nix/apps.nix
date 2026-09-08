{
  projectHsPackages,
  mkHsPkgs,
}: pkgs: let
  hsPkgs = mkHsPkgs pkgs;
  resolveProgressExe = pkgs.lib.getExe' hsPkgs.aihc-resolve-tooling-common "resolve-progress";
  resolveExtensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-resolve-tooling-common "resolve-extension-progress";
  tcProgressExe = pkgs.lib.getExe' hsPkgs.aihc-tc-tooling-common "tc-progress";
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
