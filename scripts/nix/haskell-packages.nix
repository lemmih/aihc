{
  projectHsPackages,
  sources,
}: let
  checkedPackageNames = [
    "aihc"
    "aihc-fmt"
    "aihc-haddock"
    "aihc-resolve"
    "aihc-tc"
    "aihc-testing"
  ];

  componentSpecs = {
    aihc-resolve = {
      src = sources.resolveSrc;
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-tc = {
      src = sources.tcSrc;
      cabal2nixOptions = {
        extraCabal2nixOptions = "--flag fuzz --subpath components/aihc-tc";
        srcModifier = src: src;
      };
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-prim = {
      src = sources.primSrc;
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-internal = {
      src = sources.internalSrc;
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-template-haskell = {
      src = sources.templateHaskellSrc;
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-testing = {
      src = sources.testingSrc;
      cabal2nixOptions = {
        extraCabal2nixOptions = "--subpath tooling/aihc-testing";
        srcModifier = src: src;
      };
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-resolve-tooling-common = {
      src = sources.resolveToolingCommonSrc;
      cabal2nixOptions = {
        extraCabal2nixOptions = "--subpath tooling/aihc-resolve-tooling-common";
        srcModifier = src: src;
      };
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-tc-tooling-common = {
      src = sources.tcToolingCommonSrc;
      cabal2nixOptions = {
        extraCabal2nixOptions = "--subpath tooling/aihc-tc-tooling-common";
        srcModifier = src: src;
      };
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc = {
      src = sources.aihcSrc;
      cabal2nixOptions = {
        extraCabal2nixOptions = "--subpath bin/aihc";
        srcModifier = src: src;
      };
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-fmt = {
      src = sources.fmtSrc;
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-haddock = {
      src = sources.haddockSrc;
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
  };

  enableCoverageWithExport = hsLib: drv:
    hsLib.overrideCabal drv (old: {
      configureFlags = (old.configureFlags or []) ++ ["--enable-coverage"];
      testFlags = (old.testFlags or []) ++ ["--hide-successes" "--hedgehog-tests" "10000"];
      preCheck =
        (old.preCheck or "")
        + ''
          # GHC 9.12: test-suite mix files land in dist/build/<comp>/<comp>-tmp/extra-compilation-artifacts/
          # but hpc markup only searches dist/build/extra-compilation-artifacts/hpc/vanilla/mix/.
          # Copy all vanilla mix files from component build dirs into the searched location.
          target_mix=dist/build/extra-compilation-artifacts/hpc/vanilla/mix
          mkdir -p "$target_mix"
          while IFS= read -r mix_file; do
            rel="''${mix_file##*/extra-compilation-artifacts/hpc/vanilla/mix/}"
            pkg_dir="$target_mix/$(dirname "$rel")"
            mkdir -p "$pkg_dir"
            cp "$mix_file" "$pkg_dir/" 2>/dev/null || true
          done < <(find dist/build -path "*/extra-compilation-artifacts/hpc/vanilla/mix/*.mix" 2>/dev/null)
        '';
      postInstall =
        (old.postInstall or "")
        + ''
          # Export HPC coverage data.
          if [ -d dist/hpc ]; then
            mkdir -p "$out/hpc"
            cp -r dist/hpc/* "$out/hpc/"
          fi

          target_mix=dist/build/extra-compilation-artifacts/hpc/vanilla/mix
          if [ -d "$target_mix" ]; then
            mkdir -p "$out/hpc/vanilla/mix"
            cp -r "$target_mix"/* "$out/hpc/vanilla/mix/"
          fi
        '';
    });

  applyHaddockMode = hsLib: mode: drv:
    if mode == "do"
    then hsLib.doHaddock drv
    else if mode == "dont"
    then hsLib.dontHaddock drv
    else drv;

  isOverridableHaskellDrv = pkgs: drv:
    pkgs.lib.isDerivation drv && drv.isHaskellLibrary or false;

  disableUpstreamChecks = pkgs: hsLib: localPackageNames: _final: prev:
    builtins.mapAttrs (
      name: drv:
        if builtins.elem name localPackageNames || !(isOverridableHaskellDrv pkgs drv)
        then drv
        else
          hsLib.dontCheck
          (hsLib.dontHaddock
            (hsLib.disableExecutableProfiling (hsLib.disableLibraryProfiling drv)))
    )
    prev;

  mkHackageLibrary = hsLib: drv:
    hsLib.dontCheck (hsLib.dontHaddock (
      hsLib.disableExecutableProfiling (hsLib.disableLibraryProfiling drv)
    ));
in rec {
  # Hackage dependencies whose build settings need manual adjustment.
  hackageDepTestFixes = pkgs: _final: prev: {
    network = pkgs.haskell.lib.dontCheck prev.network;
  };

  mkHsPkgsVariant = pkgs: {
    disableOptimization ? false,
    enableDocs ? false,
    enableCoverage ? false,
    enableSeparateIntermediates ? false,
    warningsAsErrors ? false,
  }: let
    hsLib = pkgs.haskell.lib;
    localPackageNames = (builtins.attrNames componentSpecs) ++ ["aihc-hackage" "aihc-package-plan"];
    enableWarningsAsErrors = drv:
      if warningsAsErrors
      then
        hsLib.overrideCabal drv (old: {
          configureFlags = (old.configureFlags or []) ++ ["--ghc-options=-Werror"];
        })
      else drv;

    mkComponent = final: name: spec: let
      prepareChecks = enableSeparateIntermediates && builtins.elem name checkedPackageNames;
      baseDrv =
        final.callCabal2nixWithOptions
        name
        (spec.src pkgs)
        (spec.cabal2nixOptions or "")
        {};
      profilingAdjusted =
        if spec.disableProfiling
        then hsLib.disableExecutableProfiling (hsLib.disableLibraryProfiling baseDrv)
        else baseDrv;
      optimizationAdjusted =
        if disableOptimization && spec.optimizeForChecks
        then hsLib.disableOptimization profilingAdjusted
        else profilingAdjusted;
      coverageAdjusted =
        if enableCoverage && spec.supportsCoverage
        then enableCoverageWithExport hsLib optimizationAdjusted
        else optimizationAdjusted;
      warningsAdjusted = enableWarningsAsErrors coverageAdjusted;
      intermediatesAdjusted =
        if prepareChecks
        then
          hsLib.dontHaddock (
            hsLib.overrideCabal warningsAdjusted (_old: {
              doInstallIntermediates = true;
              enableSeparateIntermediatesOutput = true;
            })
          )
        else warningsAdjusted;
      checksAdjusted =
        if prepareChecks
        then
          hsLib.overrideCabal intermediatesAdjusted (_old: {
            # Build test components into the reusable intermediates, but leave
            # execution to the independently scheduled check derivations.
            doCheck = true;
            testFlags = ["--pattern" "__nix-build-tests-without-running__"];
          })
        else hsLib.dontCheck intermediatesAdjusted;
      haddockMode =
        if enableDocs
        then
          if spec.supportsDocs
          then "do"
          else "dont"
        else "leave";
    in
      applyHaddockMode hsLib haddockMode checksAdjusted;
  in
    (projectHsPackages pkgs).override {
      overrides = final: prev:
        disableUpstreamChecks pkgs hsLib localPackageNames final prev
        // hackageDepTestFixes pkgs final prev
        // {
          aihc-cpp = mkHackageLibrary hsLib (final.callHackageDirect {
            pkg = "aihc-cpp";
            ver = "1.0.0.2";
            sha256 = "1bsq5549wq9nz62qrij6iabac4xv57dbwcqnflgvbfimj910jcz6";
          } {});
          aihc-parser = mkHackageLibrary hsLib (final.callHackageDirect {
            pkg = "aihc-parser";
            ver = "3.0.0.0";
            sha256 = "1xm65y3h2r1fyjdkanns5nvvdbvsl4nhmkhjp09531bhnn30l5j4";
          } {});
          aihc-hackage = hsLib.dontCheck (hsLib.dontHaddock (
            hsLib.disableExecutableProfiling (hsLib.disableLibraryProfiling (
              final.callCabal2nix "aihc-hackage" (sources.hackageSrc pkgs) {}
            ))
          ));
          aihc-package-plan = hsLib.dontCheck (hsLib.dontHaddock (
            hsLib.disableExecutableProfiling (hsLib.disableLibraryProfiling (
              final.callCabal2nix "aihc-package-plan" (sources.packagePlanSrc pkgs) {}
            ))
          ));
        }
        // builtins.mapAttrs (mkComponent final) componentSpecs;
    };

  mkHsPkgs = pkgs: mkHsPkgsVariant pkgs {};

  mkHsPkgsForChecks = pkgs:
    mkHsPkgsVariant pkgs {
      enableSeparateIntermediates = true;
      warningsAsErrors = true;
    };
}
