{root}: let
  matchesSuffix = pkgs: suffixes: path: let
    baseName = baseNameOf path;
  in
    builtins.any (suffix: pkgs.lib.hasSuffix suffix baseName) suffixes;

  mkComponentSrc = subpath: suffixes: pkgs:
    pkgs.lib.cleanSourceWith {
      src = root + subpath;
      filter = path: type: let
        baseName = baseNameOf path;
        matchesSourceSuffix = matchesSuffix pkgs suffixes path;
      in
        type == "directory" || matchesSourceSuffix || baseName == "LICENSE" || baseName == "CHANGELOG.md";
    };

  mkRootSubsetSrc = prefixes: suffixes: pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
        relPath = pkgs.lib.removePrefix ((toString root) + "/") (toString path);
        inSubset = builtins.any (prefix: pkgs.lib.hasPrefix prefix relPath) prefixes;
        matchesSourceSuffix = matchesSuffix pkgs suffixes path;
      in
        type == "directory" || (inSubset && (matchesSourceSuffix || baseName == "LICENSE" || baseName == "CHANGELOG.md"));
    };

  exampleSourceSuffixes = [
    ".hs"
    ".hs-boot"
    ".cabal"
    "args"
    "exit"
    "exit.ghc"
    "exit.wasm32-wasip3"
    "stdin"
    "stderr"
    "stdout"
  ];
in rec {
  # Source filtering: only include relevant files for each component.
  # This prevents rebuilds when unrelated files change.
  evalFixturesSrc = mkComponentSrc "/test/Test/Fixtures/eval" [
    ".yaml"
    ".yml"
  ];

  resolveSrc = mkComponentSrc "/components/aihc-resolve" [
    ".hs"
    ".hs-boot"
    ".cabal"
    ".yaml"
    ".yml"
  ];

  tcSrc =
    mkRootSubsetSrc [
      "components/aihc-tc/"
      "core-libs/aihc-prim/src/GHC/Classes.hs"
      "core-libs/aihc-prim/src/GHC/Prim.hs"
      "core-libs/aihc-prim/src/GHC/Prim/Base.hs"
      "core-libs/aihc-prim/src/GHC/Prim/IO.hs"
      "core-libs/aihc-prim/src/GHC/Tuple.hs"
      "core-libs/aihc-prim/src/GHC/Types.hs"
    ] [
      ".hs"
      ".hs-boot"
      ".cabal"
      ".yaml"
      ".yml"
    ];

  baseSrc = mkComponentSrc "/core-libs/aihc-base" [
    ".hs"
    ".hs-boot"
    ".cabal"
  ];

  hackageSrc = mkComponentSrc "/tooling/aihc-hackage" [
    ".hs"
    ".hs-boot"
    ".cabal"
  ];

  testingSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
        relPath = pkgs.lib.removePrefix ((toString root) + "/") (toString path);
        inTesting = pkgs.lib.hasPrefix "tooling/aihc-testing/" relPath;
        matchesSourceSuffix = matchesSuffix pkgs [".hs" ".hs-boot" ".cabal"] path;
      in
        type == "directory" || (inTesting && (matchesSourceSuffix || baseName == "LICENSE" || baseName == "CHANGELOG.md"));
    };

  primSrc = mkComponentSrc "/core-libs/aihc-prim" [
    ".hs"
    ".hs-boot"
    ".cabal"
  ];

  internalSrc = mkComponentSrc "/core-libs/aihc-internal" [
    ".hs"
    ".hs-boot"
    ".cabal"
  ];

  systemCxxStdLibSrc = mkComponentSrc "/core-libs/system-cxx-std-lib" [
    ".cabal"
  ];

  templateHaskellSrc = mkComponentSrc "/core-libs/aihc-template-haskell" [
    ".hs"
    ".hs-boot"
    ".cabal"
    "LICENSE.filepath"
  ];

  resolveToolingCommonSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        relPath = pkgs.lib.removePrefix ((toString root) + "/") (toString path);
        inToolingCommon = pkgs.lib.hasPrefix "tooling/aihc-resolve-tooling-common/" relPath;
        inResolveCommon = pkgs.lib.hasPrefix "components/aihc-resolve/common/" relPath;
        matchesSourceSuffix = matchesSuffix pkgs [".hs" ".hs-boot" ".cabal"] path;
      in
        type == "directory" || ((inToolingCommon || inResolveCommon) && matchesSourceSuffix);
    };

  aihcSrc =
    mkRootSubsetSrc [
      "bin/aihc/"
      "core-libs/aihc-prim/src/GHC/Classes.hs"
      "core-libs/aihc-prim/src/GHC/Prim.hs"
      "core-libs/aihc-prim/src/GHC/Tuple.hs"
      "core-libs/aihc-prim/src/GHC/Types.hs"
    ] [
      ".hs"
      ".hs-boot"
      ".cabal"
      ".c"
      ".h"
      ".wit"
      ".yaml"
      ".yml"
      ".fc"
      ".lir"
      # The golden assembly of the native backends.
      ".s"
      "expected.txt"
    ];

  examplesSrc = mkRootSubsetSrc ["examples/"] exampleSourceSuffixes;

  coreLibrariesSrc = mkRootSubsetSrc ["core-libs/"] exampleSourceSuffixes;

  exampleSrc = exampleName:
    mkRootSubsetSrc ["examples/${exampleName}/"] exampleSourceSuffixes;

  fmtSrc = mkComponentSrc "/bin/aihc-fmt" [
    ".hs"
    ".hs-boot"
    ".cabal"
    ".yaml"
    ".yml"
  ];

  # Filtered source for nix linting - only nix files.
  nixSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
      in
        type == "directory" || pkgs.lib.hasSuffix ".nix" baseName;
    };

  # Filtered source for Haskell linting/formatting - .hs files and .cabal files in components, tooling, and bin.
  # (.cabal files needed for ormolu to detect language settings like GHC2021)
  haskellSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
        pathStr = toString path;
        isHaskell = pkgs.lib.hasSuffix ".hs" baseName;
        isCabal = pkgs.lib.hasSuffix ".cabal" baseName;
        isHlintConfig = baseName == ".hlint.yaml";
        isFixture = pkgs.lib.hasInfix "/test/Test/Fixtures/" pathStr;
        inComponents = pkgs.lib.hasInfix "/components/" pathStr;
        inTooling = pkgs.lib.hasInfix "/tooling/" pathStr;
        inBin = pkgs.lib.hasInfix "/bin/" pathStr;
        inCoreLibs = pkgs.lib.hasInfix "/core-libs/" pathStr;
        inNixHaskell = pkgs.lib.hasInfix "/scripts/nix/ucd2haskell-aihc/" pathStr;
      in
        type == "directory" || isHlintConfig || ((inComponents || inTooling || inBin || inCoreLibs || inNixHaskell) && (isCabal || (isHaskell && !isFixture)));
    };

  # Cabal formatting should not be invalidated by ordinary Haskell changes.
  cabalSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type:
        type == "directory" || pkgs.lib.hasSuffix ".cabal" (baseNameOf path);
    };

  # Filtered source for C linting/formatting, including tool configuration.
  cSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
        relPath = pkgs.lib.removePrefix ((toString root) + "/") (toString path);
        isBuildOutput = relPath == "dist-newstyle" || pkgs.lib.hasPrefix "dist-newstyle/" relPath;
        isCSource =
          pkgs.lib.hasSuffix ".c" baseName
          || pkgs.lib.hasSuffix ".h" baseName
          || pkgs.lib.hasSuffix ".wit" baseName;
        isCConfig = baseName == ".clang-format" || baseName == ".clang-tidy";
      in
        !isBuildOutput && (type == "directory" || isCSource || isCConfig);
    };

  # Filtered source for scripts - only shell scripts.
  scriptsSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
      in
        type == "directory" || (pkgs.lib.hasSuffix ".sh" baseName && pkgs.lib.hasInfix "/scripts" (toString path));
    };
}
