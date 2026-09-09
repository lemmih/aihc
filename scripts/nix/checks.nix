{
  projectHsPackages,
  sources,
  mkHsPkgsForChecks,
  mkWasiSysroot,
}: pkgs: let
  hsPkgs = mkHsPkgsForChecks pkgs;
  wasmLd = pkgs.writeShellScriptBin "wasm-ld" ''
    exec ${pkgs.lld}/bin/wasm-ld "$@"
  '';
  wasmSysroot = mkWasiSysroot pkgs;
  examplesSource = sources.examplesSrc pkgs;
  exampleEntries = builtins.readDir "${examplesSource}/examples";
  allExampleNames = builtins.filter (
    name:
      exampleEntries.${name}
      == "directory"
      && builtins.pathExists "${examplesSource}/examples/${name}/Main.hs"
  ) (builtins.attrNames exampleEntries);
  # This example uses more than the temporary 100 MB heap limit.
  disabledExampleNames = ["unboxed-tail-recursion"];
  exampleNames = builtins.filter (name: !builtins.elem name disabledExampleNames) allExampleNames;
  cTidyCompilerFlags =
    ["-std=c11" "-Wall" "-Wextra" "-Wpedantic"]
    ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
      "-isysroot"
      "${pkgs.apple-sdk}/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"
    ];

  addHiddenSuccesses = old: {
    # Replace the package-build sentinel and hide passing test output.
    testFlags =
      builtins.filter
      (flag: !builtins.elem flag ["--pattern" "__nix-build-tests-without-running__"])
      (old.testFlags or [])
      ++ ["--hide-successes"];
  };

  addCheckSettings = drv: old:
    addHiddenSuccesses old
    // pkgs.lib.optionalAttrs (drv ? intermediates) {
      # Reuse the optimized build, including its already-compiled test components.
      doInstallIntermediates = false;
      enableSeparateIntermediatesOutput = false;
      previousIntermediates = drv.intermediates;
    };

  reuseCompiledTests = drv: check:
    if drv ? intermediates
    then
      check.overrideAttrs (_: {
        # The package build already compiled every test executable.
        buildPhase = ''
          runHook preBuild
          rm -rf dist/build
          cp -R ${drv.intermediates}/share/haskell/${drv.compiler.version}/${drv.pname}-${drv.version}/dist/build dist/build
          chmod -R u+w dist/build
          ./Setup register --inplace
          runHook postBuild
        '';
        # These outputs only record successful tests.
        installPhase = ''
          runHook preInstall
          for outputName in $outputs; do
            mkdir -p "''${!outputName}"
          done
          runHook postInstall
        '';
      })
    else check;

  mkPackageTest = drv:
    reuseCompiledTests drv (pkgs.haskell.lib.doCheck (
      pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.overrideCabal drv (addCheckSettings drv))
    ));

  mkEvalPackageTest = drv:
    reuseCompiledTests drv (pkgs.haskell.lib.doCheck (
      pkgs.haskell.lib.dontHaddock (
        pkgs.haskell.lib.overrideCabal drv (
          old:
            addCheckSettings drv old
            // {
              preCheck =
                (old.preCheck or "")
                + ''
                  export AIHC_BASE_SRC=${sources.baseSrc pkgs}
                  export AIHC_PRIM_SRC=${sources.primSrc pkgs}
                  export AIHC_EVAL_FIXTURES=${sources.evalFixturesSrc pkgs}
                '';
            }
        )
      )
    ));

  mkAihcPackageTest = suite: drv:
    reuseCompiledTests drv (pkgs.haskell.lib.doCheck (
      pkgs.haskell.lib.dontHaddock (
        pkgs.haskell.lib.overrideCabal drv (
          old:
            addCheckSettings drv old
            // {
              testTargets = [suite];
              # Eight test workers share the compiler test workload.
              # Keep the capability count equal to the test worker count.
              # Record garbage collection statistics for CI performance checks.
              testFlags = (addHiddenSuccesses old).testFlags ++ ["--num-threads" "8" "+RTS" "-N8" "-A32m" "-s" "-RTS"];
              # The C toolchain is only needed while the tests run. Adding it to
              # testToolDepends would append --extra-include-dirs/--extra-lib-dirs
              # to the configure flags, which changes GHC's flag hash and forces a
              # full recompilation instead of reusing the package intermediates.
              preCheck =
                (old.preCheck or "")
                + ''
                  # The LLVM bintools must shadow the GNU binutils that the clang wrapper
                  # links into its own bin directory: the foreign-target suites
                  # archive their objects with llvm-ar.
                  export PATH=${pkgs.llvmPackages.bintools}/bin:${pkgs.llvmPackages.clang}/bin:$PATH
                  coreLibsRoot="$TMPDIR/aihc-core-libs-root"
                  mkdir -p "$coreLibsRoot/core-libs"
                  ln -sfn ${sources.baseSrc pkgs} "$coreLibsRoot/core-libs/aihc-base"
                  ln -sfn ${sources.primSrc pkgs} "$coreLibsRoot/core-libs/aihc-prim"
                  ln -sfn ${sources.internalSrc pkgs} "$coreLibsRoot/core-libs/aihc-internal"
                  ln -sfn ${sources.templateHaskellSrc pkgs} "$coreLibsRoot/core-libs/aihc-template-haskell"
                  ln -sfn ${sources.systemCxxStdLibSrc pkgs} "$coreLibsRoot/core-libs/system-cxx-std-lib"
                  export AIHC_CORE_LIBS_ROOT="$coreLibsRoot"
                  export AIHC_BASE_SRC="$coreLibsRoot/core-libs/aihc-base"
                  export AIHC_PRIM_SRC="$coreLibsRoot/core-libs/aihc-prim"
                  export AIHC_EVAL_FIXTURES=${sources.evalFixturesSrc pkgs}
                  export AIHC_TEST_ROOT=${sources.aihcSrc pkgs}
                  ${pkgs.lib.optionalString (suite == "spec") ''
                    export AIHC_PREBUILT_STORE=${specSeedStore}
                  ''}
                '';
            }
        )
      )
    ));

  mkSourceCheck = name: src: nativeBuildInputs: text:
    pkgs.runCommand name {
      inherit src nativeBuildInputs;
    } ''
      cd "$src"
      ${text}
      touch "$out"
    '';

  compilationModes = [
    {
      name = "incremental";
      flags = [];
    }
  ];
  garbageCollectors = ["semispace"];
  nativeBackendBySystem = {
    "aarch64-darwin" = "apple-arm64";
    "x86_64-linux" = "linux-amd64";
  };
  nativeBackend = nativeBackendBySystem.${pkgs.stdenv.hostPlatform.system} or null;
  backends = ["llvm"] ++ pkgs.lib.optional (nativeBackend != null) nativeBackend;
  # Test.Aihc.SeedStore installs aihc-prim for apple-arm64 and llvm always, and
  # for linux-amd64 and wasm32-wasip3 when the toolchain supports them, which it
  # does inside the sandbox. aihc-base is only needed for the target build-exe
  # compiles for, which is the host backend.
  specSeedPrimTargets =
    pkgs.lib.unique (["apple-arm64" "llvm" "linux-amd64" "wasm32-wasip3"] ++ [specSeedBaseTarget]);
  specSeedBaseTarget =
    if nativeBackend == null
    then "llvm"
    else nativeBackend;
  compilationMatrix = builtins.concatLists (
    map (
      backend:
        builtins.concatLists (
          map (compilation: map (gc: {inherit backend compilation gc;}) garbageCollectors) compilationModes
        )
    )
    backends
  );
  wasip3CompilationModes = _exampleName: compilationModes;

  renderExampleTest = {
    backend,
    compilation,
    gc,
  }: ''
    executable="$TMPDIR/$example_name-${backend}-${compilation.name}-${gc}"
    actual_stdout="$executable.stdout"
    actual_stderr="$executable.stderr"
    timeout_stderr="$executable.timeout-stderr"
    run_directory="$executable.run"
    stdin_file=/dev/null
    if [[ -f "$example_directory/stdin" ]]; then
      stdin_file="$example_directory/stdin"
    fi
    example_args=()
    if [[ -f "$example_directory/args" ]]; then
      mapfile -t example_args < "$example_directory/args"
    fi
    expected_stderr="$empty_stderr"
    if [[ -f "$example_directory/stderr" ]]; then
      expected_stderr="$example_directory/stderr"
    fi
    expected_exit=0
    if [[ -f "$example_directory/exit" ]]; then
      expected_exit=$(<"$example_directory/exit")
    fi
    if timeout --foreground --kill-after=5s 120s ${aihcExe} build-exe "$source" \
      --target ${backend} \
      --gc ${gc} \
      --store "$store" \
      --build-root "$TMPDIR/.aihc-cache" \
      "''${package_flags[@]}" \
      ${pkgs.lib.escapeShellArgs compilation.flags} \
      --output "$executable"; then
      :
    else
      compile_exit=$?
      if [[ "$compile_exit" -eq 124 || "$compile_exit" -eq 137 ]]; then
        echo "Timed out compiling $example_name/${backend}-${compilation.name}-${gc}" >&2
      else
        echo "Compiler failed for $example_name/${backend}-${compilation.name}-${gc} with exit $compile_exit" >&2
      fi
      exit "$compile_exit"
    fi
    mkdir -p "$run_directory"
    if timeout --foreground --kill-after=5s 10s \
      bash -c 'run_directory=$1; executable=$2; stderr=$3; argv0=$4; shift 4; cd "$run_directory"; exec -a "$argv0" "$executable" +RTS -M100M -RTS "$@" 2> "$stderr"' \
      bash "$run_directory" "$executable" "$actual_stderr" "$example_name" "''${example_args[@]}" \
      < "$stdin_file" > "$actual_stdout" 2> "$timeout_stderr"; then
      actual_exit=0
    else
      actual_exit=$?
    fi
    if [[ "$actual_exit" -eq 124 || "$actual_exit" -eq 137 ]]; then
      echo "Timed out running $example_name/${backend}-${compilation.name}-${gc}" >&2
      cat "$timeout_stderr" >&2
      exit 1
    fi
    if [[ "$expected_exit" == nonzero ]]; then
      if [[ "$actual_exit" -eq 0 ]]; then
        echo "Expected $example_name/${backend}-${compilation.name}-${gc} to fail" >&2
        exit 1
      fi
    elif [[ "$expected_exit" =~ ^[0-9]+$ ]]; then
      if [[ "$actual_exit" -ne "$expected_exit" ]]; then
        echo "Expected $example_name/${backend}-${compilation.name}-${gc} to exit with $expected_exit, got $actual_exit" >&2
        exit 1
      fi
    else
      echo "Invalid expected exit status for $example_name: $expected_exit" >&2
      exit 1
    fi
    diff --unified \
      --label "$example_name/stdout-expected" \
      --label "$example_name/stdout-${backend}-${compilation.name}-${gc}" \
      "$expected_stdout" "$actual_stdout"
    diff --unified \
      --label "$example_name/stderr-expected" \
      --label "$example_name/stderr-${backend}-${compilation.name}-${gc}" \
      "$expected_stderr" "$actual_stderr"
  '';

  renderWasip3ExampleTest = compilation: ''
    executable="$TMPDIR/$example_name-wasm32-wasip3-${compilation.name}.wasm"
    actual_stdout="$executable.stdout"
    actual_stderr="$executable.stderr"
    run_directory="$executable.run"
    stdin_file=/dev/null
    if [[ -f "$example_directory/stdin" ]]; then
      stdin_file="$example_directory/stdin"
    fi
    example_args=()
    if [[ -f "$example_directory/args" ]]; then
      mapfile -t example_args < "$example_directory/args"
    fi
    expected_stderr="$empty_stderr"
    if [[ -f "$example_directory/stderr" ]]; then
      expected_stderr="$example_directory/stderr"
    fi
    expected_exit=0
    if [[ -f "$example_directory/exit" ]]; then
      expected_exit=$(<"$example_directory/exit")
    fi
    if [[ -f "$example_directory/exit.wasm32-wasip3" ]]; then
      expected_exit=$(<"$example_directory/exit.wasm32-wasip3")
    fi
    if timeout --foreground --kill-after=5s 120s ${aihcExe} build-exe "$source" \
      --target wasm32-wasip3 \
      --store "$store" \
      --build-root "$TMPDIR/.aihc-cache" \
      "''${package_flags[@]}" \
      ${pkgs.lib.escapeShellArgs compilation.flags} \
      --output "$executable"; then
      :
    else
      compile_exit=$?
      if [[ "$compile_exit" -eq 124 || "$compile_exit" -eq 137 ]]; then
        echo "Timed out compiling $example_name/wasm32-wasip3-${compilation.name}" >&2
      else
        echo "Compiler failed for $example_name/wasm32-wasip3-${compilation.name} with exit $compile_exit" >&2
      fi
      exit "$compile_exit"
    fi
    mkdir -p "$run_directory"
    if timeout --foreground --kill-after=5s 30s wasmtime run -C cache=n -S cli \
      --dir "$run_directory::." \
      --argv0 "$example_name" \
      "$executable" +RTS -M100M -RTS "''${example_args[@]}" \
      < "$stdin_file" > "$actual_stdout" 2> "$actual_stderr"; then
      actual_exit=0
    else
      actual_exit=$?
    fi
    if [[ "$actual_exit" -eq 124 || "$actual_exit" -eq 137 ]]; then
      echo "Timed out running $example_name/wasm32-wasip3-${compilation.name}" >&2
      exit 1
    fi
    if [[ "$expected_exit" == nonzero ]]; then
      if [[ "$actual_exit" -eq 0 ]]; then
        echo "Expected $example_name/wasm32-wasip3-${compilation.name} to fail" >&2
        exit 1
      fi
    elif [[ "$expected_exit" =~ ^[0-9]+$ ]]; then
      if [[ "$actual_exit" -ne "$expected_exit" ]]; then
        echo "Expected $example_name/wasm32-wasip3-${compilation.name} to exit with $expected_exit, got $actual_exit" >&2
        echo "stdout:" >&2
        cat "$actual_stdout" >&2 || true
        echo "stderr:" >&2
        cat "$actual_stderr" >&2 || true
        exit 1
      fi
    else
      echo "Invalid expected exit status for $example_name: $expected_exit" >&2
      exit 1
    fi
    diff --unified \
      --label "$example_name/stdout-expected" \
      --label "$example_name/stdout-wasm32-wasip3-${compilation.name}" \
      "$expected_stdout" "$actual_stdout"
    if [[ "$expected_exit" != nonzero ]]; then
      diff --unified \
        --label "$example_name/stderr-expected" \
        --label "$example_name/stderr-wasm32-wasip3-${compilation.name}" \
        "$expected_stderr" "$actual_stderr"
    fi
  '';

  aihcExe = pkgs.writeShellScript "aihc-with-memory-limit" ''
    exec ${pkgs.lib.getExe' hsPkgs.aihc "aihc"} +RTS -M2G -A32m -RTS "$@"
  '';

  resolveTests = mkPackageTest hsPkgs.aihc-resolve;
  tcTests = mkPackageTest hsPkgs.aihc-tc;
  testingTests = mkPackageTest hsPkgs.aihc-testing;
  aihcTests = pkgs.linkFarm "aihc-tests" (map (suite: {
    name = suite;
    path = mkAihcPackageTest suite hsPkgs.aihc;
  }) ["spec" "dev-spec"]);
  fmtTests = mkPackageTest hsPkgs.aihc-fmt;
  haddockTests = mkPackageTest hsPkgs.aihc-haddock;
  unicode = import ./unicode.nix {inherit pkgs;};
  unicodeGenerated =
    pkgs.runCommand "aihc-unicode-generated" {
      nativeBuildInputs = [pkgs.diffutils pkgs.ormolu];
    } ''
      generated="$TMPDIR/generated/GHC/Prim/Unicode.hs"
      UNICODE_VERSION=${unicode.version} ${unicode.generator} \
        --input=${unicode.ucd}/ \
        --output="$TMPDIR/generated" \
        --core-prop=Uppercase \
        --core-prop=Lowercase
      ormolu --mode inplace "$generated"
      diff --unified ${sources.primSrc pkgs}/src/GHC/Prim/Unicode.hs "$generated"
      touch "$out"
    '';

  nixLint = mkSourceCheck "aihc-nix-lint" (sources.nixSrc pkgs) [pkgs.statix] ''
    statix check flake.nix
  '';

  nixFormat = mkSourceCheck "aihc-nix-format" (sources.nixSrc pkgs) [pkgs.alejandra] ''
    alejandra --check .
  '';

  haskellLint = mkSourceCheck "aihc-haskell-lint" (sources.haskellSrc pkgs) [pkgs.hlint pkgs.findutils] ''
    find . -type f -name '*.hs' -print0 \
      | xargs -0 -r hlint
  '';

  haskellFormat = mkSourceCheck "aihc-haskell-format" (sources.haskellSrc pkgs) [pkgs.ormolu pkgs.findutils] ''
    find . -type f -name '*.hs' -print0 \
      | xargs -0 -r ormolu --mode check
  '';

  cLint = mkSourceCheck "aihc-c-lint" (sources.cSrc pkgs) [pkgs.clang-tools pkgs.findutils pkgs.wit-bindgen] ''
    bindings_directory="$TMPDIR/aihc-wasip3-bindings"
    mkdir -p "$bindings_directory"
    wit-bindgen c --world command --out-dir "$bindings_directory" bin/aihc/compiler/wasm/runtime/wit
    while IFS= read -r -d "" file; do
      if [[ "$file" == *bin/aihc/compiler/wasm/runtime/*.c || "$file" == *aihc_host_wasip3.c ]]; then
        clang-tidy-unwrapped --quiet "$file" -- \
          --target=wasm32-wasip1 \
          --sysroot=${wasmSysroot} \
          -std=c11 -Wall -Wextra -Wpedantic \
          -Ibin/aihc/compiler/wasm/runtime \
          -Ibin/aihc/compiler/native/runtime \
          -isystem "$bindings_directory"
      else
        clang-tidy --quiet "$file" -- ${pkgs.lib.escapeShellArgs cTidyCompilerFlags}
      fi
    done < <(find . -type f -name '*.c' -print0)
  '';

  cFormat = mkSourceCheck "aihc-c-format" (sources.cSrc pkgs) [pkgs.clang-tools pkgs.findutils] ''
    find . -type f \( -name '*.c' -o -name '*.h' \) -print0 \
      | xargs -0 -r clang-format --dry-run --Werror
  '';

  cabalFormat = mkSourceCheck "aihc-cabal-format" (sources.cabalSrc pkgs) [pkgs.haskellPackages.cabal-gild pkgs.findutils] ''
    failed=0
    while IFS= read -r -d "" file; do
      cabal-gild --mode check --input "$file" || failed=1
    done < <(find . -type f -name '*.cabal' -print0)
    test "$failed" -eq 0
  '';

  coreLibrariesInstall =
    pkgs.runCommand "aihc-core-libraries-install" {
      src = sources.coreLibrariesSrc pkgs;
      nativeBuildInputs = [
        pkgs.findutils
        pkgs.llvmPackages.bintools
        pkgs.llvmPackages.clang
      ];
    } ''
      cd "$src"
      export GHCRTS=-N2
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      store="$TMPDIR/store"
      mkdir -p "$store"

      ${aihcExe} install core-libs/aihc-prim --store "$store" --keep-core --keep-grin --lint --target apple-arm64

      test -n "$(find "$store" -path '*/GHC/Prim/core' -print -quit)"
      test -n "$(find "$store" -path '*/GHC/Prim/grin' -print -quit)"
      test -n "$(find "$store" -path '*/GHC/Prim/GHC.Prim.o' -print -quit)"
      test -n "$(find "$store" -path '*/lib/libaihc-prim.a' -print -quit)"
      test -z "$(find "$store" -type f -name 'core.bad' -print -quit)"

      ${aihcExe} install core-libs/aihc-template-haskell --store "$store" --keep-core --lint --target apple-arm64

      test -n "$(find "$store" -path '*/Language/Haskell/TH/core' -print -quit)"
      test -n "$(find "$store" -path '*/GHC/Internal/TH/Syntax/GHC.Internal.TH.Syntax.o' -print -quit)"
      archive="$(find "$store" -path '*/lib/libaihc-template-haskell.a' -print -quit)"
      test -n "$archive"
      test -s "$archive"
      test -z "$(find "$store" -type f -name 'core.bad' -print -quit)"
      touch "$out"
    '';

  # The store the aihc test suite works against. Installing anything into an
  # empty store compiles aihc-prim first, and the build-exe tests additionally
  # need aihc-base; the suite used to pay that per test, which was most of what
  # it allocated. Building the store here instead hands the tests a warm one
  # through AIHC_PREBUILT_STORE and keeps the result in the Nix cache across
  # runs. Outside CI the suite installs the same libraries itself, once, from a
  # tasty resource. The target list must cover everything Test.Aihc.SeedStore
  # asks for; a superset is harmless, a missing target only makes the tests
  # install it again.
  # These fresh Nix stores do not need a second module cache.
  # --reinstall retains installed interfaces and skips that cache.
  mkSpecSeedStore = name: script:
    pkgs.runCommand name {
      src = sources.coreLibrariesSrc pkgs;
      nativeBuildInputs = [
        pkgs.llvmPackages.bintools
        pkgs.llvmPackages.clang
        pkgs.llvmPackages.clang-unwrapped
        pkgs.wasm-tools
        pkgs.wit-bindgen
        wasmLd
      ];
    } ''
      cd "$src"
      export GHCRTS=-N2
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      export AIHC_WASM_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
      export AIHC_WASM_SYSROOT=${wasmSysroot}
      ${script}
    '';

  specPrimStoreFor = target:
    mkSpecSeedStore "aihc-spec-prim-store-${target}" ''
      ${aihcExe} install --reinstall core-libs/aihc-prim --store "$out" --target ${target}
    '';

  specSeedStore = mkSpecSeedStore "aihc-spec-seed-store" ''
    mkdir -p "$out/prim"
    ${pkgs.lib.concatMapStringsSep "\n" (target: ''
        cp -R --no-preserve=mode ${specPrimStoreFor target}/. "$out/prim/"
      '')
      specSeedPrimTargets}
    cp -R --no-preserve=mode "$out/prim" "$out/core"
    ${aihcExe} install --reinstall core-libs/aihc-base --store "$out/core" --target ${specSeedBaseTarget}
  '';

  # The compiler owns preparation of the installed toolchain. Runtime archives
  # are built once per backend/GC pair, and ordinary package installation emits
  # the reusable library interfaces and target-specific archives.
  #
  # Each target has its own derivation and output paths.
  # Nix can compile these targets at the same time.
  exampleToolchainFor = exampleToolchainWith "";

  # The macOS SDK headers, fetched the way nixpkgs fetches them for its own
  # Darwin toolchain: a fixed-output download of Apple's Command Line Tools
  # package, so it builds on any host and comes from the public cache. With
  # it, a Linux host compiles the C runtime and package C sources for
  # apple-arm64. The unwrapped Clang is used because the nixpkgs wrapper
  # injects Linux arguments that Clang rejects for a Darwin target.
  appleSdkVersions = builtins.fromJSON (builtins.readFile "${pkgs.path}/pkgs/by-name/ap/apple-sdk/metadata/versions.json");
  appleSdk = pkgs.callPackage "${pkgs.path}/pkgs/by-name/ap/apple-sdk/common/fetch-sdk.nix" {} appleSdkVersions."15";
  crossSetupFor = target:
    if target == "apple-arm64"
    then ''
      export AIHC_APPLE_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
      export AIHC_APPLE_SDK=${appleSdk}
    ''
    else "";

  exampleToolchainWith = extraSetup: target:
    pkgs.runCommand "aihc-example-toolchain-${target}" {
      src = sources.coreLibrariesSrc pkgs;
      nativeBuildInputs = [
        pkgs.llvmPackages.bintools
        pkgs.llvmPackages.clang
        pkgs.llvmPackages.clang-unwrapped
        pkgs.wasm-tools
        pkgs.wit-bindgen
        wasmLd
      ];
    } ''
      cd "$src"
      export GHCRTS=-N2
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      export AIHC_WASM_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
      export AIHC_WASM_SYSROOT=${wasmSysroot}
      ${extraSetup}
      mkdir -p "$out"

      ${aihcExe} prepare-runtime --target ${target} --gc semispace --store "$out"
      ${aihcExe} install --reinstall core-libs/aihc-base --store "$out" --lint --target ${target}

      test -n "$(find "$out" -type f -name 'package.json' -print -quit)"
      test -n "$(find "$out" -type f -name 'libaihc-base.a' -print -quit)"
      test -n "$(find "$out" -type f -name 'entry.a' -print -quit)"
    '';

  templateHaskellToolchainFor = target:
    pkgs.runCommand "aihc-template-haskell-toolchain-${target}" {
      src = sources.coreLibrariesSrc pkgs;
      nativeBuildInputs = [
        pkgs.llvmPackages.bintools
        pkgs.llvmPackages.clang
        pkgs.llvmPackages.clang-unwrapped
        pkgs.wasm-tools
        pkgs.wit-bindgen
        wasmLd
      ];
    } ''
      cd "$src"
      export GHCRTS=-N2
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      export AIHC_WASM_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
      export AIHC_WASM_SYSROOT=${wasmSysroot}
      cp -R --no-preserve=mode ${exampleToolchainFor target} "$out"
      ${aihcExe} install --reinstall core-libs/aihc-internal --store "$out" --lint --target ${target}
      ${aihcExe} install --reinstall core-libs/aihc-template-haskell --store "$out" --lint --target ${target}
      test -n "$(find "$out" -type f -name 'libaihc-template-haskell.a' -print -quit)"
      test -z "$(find "$out" -type f -name 'core.bad' -print -quit)"
    '';

  hackage = import ./hackage-packages.nix;
  hackageInstallTargets = ["llvm"] ++ pkgs.lib.optional (nativeBackend != null) nativeBackend;
  exampleExtraHackagePackages = {
    bytestring = ["deepseq" "bytestring"];
    containers = ["deepseq" "array" "containers"];
    pretty = ["deepseq" "pretty"];
  };
  findHackagePackage = name:
    pkgs.lib.findFirst (package: package.name == name) (throw "missing Hackage package ${name}") hackage.packages;

  # Retain one package store per target. Dependent checks reuse this output.
  hackageStoreFor = target: package: let
    src = hackage.fetchPackage pkgs package;
    dependencies = package.dependencies or [];
    # Reuse registered dependencies with the same source and version.
    reusableDependencies = builtins.filter (entry:
      builtins.any (dependency:
        entry.name
        == dependency.name
        && entry.version == dependency.version
        && entry.hash == dependency.hash)
      dependencies)
    hackage.packages;
    lintFlag = pkgs.lib.optionalString (package.lint or true) "--lint";
    linkWorkspaceEntry = entry: ''
      ln -sfn ${hackage.fetchPackage pkgs entry} "$workspace/${entry.name}"
    '';
  in
    pkgs.runCommand "aihc-hackage-install-${package.name}-${package.version}-${target}" {
      nativeBuildInputs = [
        pkgs.findutils
        pkgs.llvmPackages.bintools
        pkgs.llvmPackages.clang
        pkgs.llvmPackages.clang-unwrapped
        pkgs.wasm-tools
        pkgs.wit-bindgen
        wasmLd
      ];
    } ''
      set -euo pipefail
      export GHCRTS=-N2
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      export AIHC_WASM_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
      export AIHC_WASM_SYSROOT=${wasmSysroot}
      coreLibsRoot="$TMPDIR/aihc-core-libs-root"
      mkdir -p "$coreLibsRoot"
      ln -sfn ${sources.coreLibrariesSrc pkgs}/core-libs "$coreLibsRoot/core-libs"
      export AIHC_CORE_LIBS_ROOT="$coreLibsRoot"
      cp -R --no-preserve=mode ${
        if package.templateHaskell or false
        then templateHaskellToolchainFor target
        else exampleToolchainFor target
      } "$out"
      ${pkgs.lib.concatMapStringsSep "\n" (dependency: ''
          cp -R --no-preserve=mode ${hackageStoreFor target dependency}/. "$out/"
        '')
        reusableDependencies}
      workspace="$TMPDIR/workspace"
      mkdir -p "$workspace"
      ln -sfn ${src} "$workspace/${package.name}"
      ${pkgs.lib.concatMapStrings linkWorkspaceEntry dependencies}

      install_output=$(${aihcExe} install --reinstall "$workspace/${package.name}" --store "$out" ${lintFlag} --target ${target})
      printf '%s\n' "$install_output"
      test -s "''${install_output#store: }/lib/lib${package.name}.a"
      test -z "$(find "$out" -type f -name 'core.bad' -print -quit)"
    '';

  hackageInstallCases = pkgs.lib.concatMap (package:
    map (target: {
      name = "${package.name}-${package.version}-${target}";
      path = hackageStoreFor target package;
    }) (package.targets or hackageInstallTargets))
  hackage.packages;

  # Each package and target has an independent installation check.
  hackageInstallTests = assert hackage.packages != [];
    pkgs.linkFarm "aihc-hackage-install-tests" hackageInstallCases;

  exampleStoreFor = target: exampleName: let
    extraNames = exampleExtraHackagePackages.${exampleName} or [];
  in
    if extraNames == []
    then exampleToolchainFor target
    else
      pkgs.runCommand "aihc-example-store-${exampleName}-${target}" {} ''
        mkdir -p "$out"
        ${pkgs.lib.concatMapStringsSep "\n" (name: ''
            cp -R --no-preserve=mode ${hackageStoreFor target (findHackagePackage name)}/. "$out/"
          '')
          extraNames}
      '';

  exampleTestInputs = [
    pkgs.coreutils
    pkgs.diffutils
    pkgs.findutils
    pkgs.llvmPackages.bintools
    pkgs.llvmPackages.clang
  ];

  mkExampleExtraInstall = {
    toolchain,
    targets,
  }: exampleName: let
    extraNames = exampleExtraHackagePackages.${exampleName} or [];
    extraPackages = map findHackagePackage extraNames;
    linkWorkspaceEntry = package: ''
      ln -sfn ${hackage.fetchPackage pkgs package} "$workspace/${package.name}"
    '';
    installExtraForTarget = target:
      pkgs.lib.concatMapStringsSep "\n" (package: ''
        ${aihcExe} install "$workspace/${package.name}" --store "$store" --target ${target}
      '')
      extraPackages;
  in
    if extraNames == []
    then ''
      store=${toolchain}
    ''
    else ''
      store="$TMPDIR/example-store"
      cp -R --no-preserve=mode ${toolchain} "$store"
      coreLibsRoot="$TMPDIR/aihc-core-libs-root"
      mkdir -p "$coreLibsRoot"
      ln -sfn ${sources.coreLibrariesSrc pkgs}/core-libs "$coreLibsRoot/core-libs"
      export AIHC_CORE_LIBS_ROOT="$coreLibsRoot"
      workspace="$TMPDIR/workspace"
      mkdir -p "$workspace"
      ${pkgs.lib.concatMapStrings linkWorkspaceEntry extraPackages}
      ${pkgs.lib.concatMapStringsSep "\n" installExtraForTarget targets}
    '';

  mkExampleTest = exampleName: target: let
    extraNames = exampleExtraHackagePackages.${exampleName} or [];
    packageFlags =
      pkgs.lib.concatMapStringsSep " " (name: "--package ${pkgs.lib.escapeShellArg name}") extraNames;
  in
    mkSourceCheck "aihc-example-${exampleName}-${target}" (sources.exampleSrc exampleName pkgs) exampleTestInputs ''
      set -euo pipefail
      export GHCRTS=-N1
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      empty_stderr="$TMPDIR/empty-stderr"
      touch "$empty_stderr"
      package_flags=(${packageFlags})

      source="examples/${exampleName}/Main.hs"
      example_directory=$(dirname "$source")
      example_name=${pkgs.lib.escapeShellArg exampleName}
      expected_stdout="$example_directory/stdout"
      if [[ ! -f "$expected_stdout" ]]; then
        echo "Missing expected stdout for $source: $expected_stdout" >&2
        exit 1
      fi

      store=${exampleStoreFor target exampleName}
      ${pkgs.lib.concatMapStringsSep "\n" renderExampleTest (builtins.filter (entry: entry.backend == target) compilationMatrix)}
      touch "$out"
    '';

  exampleCases = pkgs.lib.concatMap (exampleName:
    map (target: {
      name = "${exampleName}-${target}";
      path = mkExampleTest exampleName target;
    })
    backends)
  exampleNames;

  mkGhcExampleTest = exampleName:
    mkSourceCheck "aihc-ghc-example-${exampleName}" (sources.exampleSrc exampleName pkgs) [pkgs.coreutils pkgs.diffutils (projectHsPackages pkgs).ghc] ''
      set -euo pipefail
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      empty_stderr="$TMPDIR/empty-stderr"
      touch "$empty_stderr"

      source="examples/${exampleName}/Main.hs"
      example_directory=$(dirname "$source")
      example_name=${pkgs.lib.escapeShellArg exampleName}
      expected_stdout="$example_directory/stdout"
      if [[ ! -f "$expected_stdout" ]]; then
        echo "Missing expected stdout for $source: $expected_stdout" >&2
        exit 1
      fi

      stdin_file=/dev/null
      if [[ -f "$example_directory/stdin" ]]; then
        stdin_file="$example_directory/stdin"
      fi
      example_args=()
      if [[ -f "$example_directory/args" ]]; then
        mapfile -t example_args < "$example_directory/args"
      fi
      expected_stderr="$empty_stderr"
      if [[ -f "$example_directory/stderr" ]]; then
        expected_stderr="$example_directory/stderr"
      fi
      expected_exit=0
      if [[ -f "$example_directory/exit" ]]; then
        expected_exit=$(<"$example_directory/exit")
      fi
      if [[ -f "$example_directory/exit.ghc" ]]; then
        expected_exit=$(<"$example_directory/exit.ghc")
      fi

      actual_stdout="$TMPDIR/$example_name.stdout"
      actual_stderr="$TMPDIR/$example_name.stderr"
      executable="$TMPDIR/$example_name-ghc"
      ghc_output_directory="$TMPDIR/$example_name-ghc-build"
      run_directory="$TMPDIR/$example_name.run"
      mkdir -p "$ghc_output_directory" "$run_directory"
      if timeout --foreground --kill-after=5s 120s \
        ghc -v0 -package-env - -outputdir "$ghc_output_directory" "$source" -o "$executable"; then
        :
      else
        compile_exit=$?
        echo "GHC failed to compile $example_name with exit $compile_exit" >&2
        exit "$compile_exit"
      fi
      if timeout --foreground --kill-after=5s 120s \
        bash -c 'run_directory=$1; executable=$2; argv0=$3; shift 3; cd "$run_directory"; exec -a "$argv0" "$executable" "$@"' \
        bash "$run_directory" "$executable" "$example_name" "''${example_args[@]}" \
        < "$stdin_file" > "$actual_stdout" 2> "$actual_stderr"; then
        actual_exit=0
      else
        actual_exit=$?
      fi

      if [[ "$actual_exit" -eq 124 || "$actual_exit" -eq 137 ]]; then
        echo "Timed out running $example_name with GHC" >&2
        cat "$actual_stderr" >&2
        exit 1
      fi
      if [[ "$expected_exit" == nonzero ]]; then
        if [[ "$actual_exit" -eq 0 ]]; then
          echo "Expected $example_name/GHC to fail" >&2
          exit 1
        fi
      elif [[ "$expected_exit" =~ ^[0-9]+$ ]]; then
        if [[ "$actual_exit" -ne "$expected_exit" ]]; then
          echo "Expected $example_name/GHC to exit with $expected_exit, got $actual_exit" >&2
          cat "$actual_stderr" >&2
          exit 1
        fi
      else
        echo "Invalid expected exit status for $example_name: $expected_exit" >&2
        exit 1
      fi

      diff --unified \
        --label "$example_name/stdout-expected" \
        --label "$example_name/stdout-ghc" \
        "$expected_stdout" "$actual_stdout"
      # GHC's uncaught-exception diagnostics vary by version and platform.
      # For expected failures, stdout and the nonzero status are the stable contract.
      if [[ "$expected_exit" != nonzero ]]; then
        diff --unified \
          --label "$example_name/stderr-expected" \
          --label "$example_name/stderr-ghc" \
          "$expected_stderr" "$actual_stderr"
      fi
      touch "$out"
    '';

  ghcExampleCases =
    map (exampleName: {
      name = exampleName;
      path = mkGhcExampleTest exampleName;
    })
    exampleNames;

  ghcExampleTest = assert exampleNames != [];
    pkgs.linkFarm "aihc-ghc-example-test" ghcExampleCases;

  # Every example uses LLVM and the available host-native backend. Nix
  # schedules independent examples in parallel against the immutable shared
  # library and runtime artifacts.
  examplesTests = assert exampleNames != [];
    pkgs.linkFarm "aihc-examples-tests" exampleCases;

  wasip3ExampleInputs = [
    pkgs.coreutils
    pkgs.diffutils
    pkgs.findutils
    pkgs.llvmPackages.bintools
    pkgs.llvmPackages.clang-unwrapped
    pkgs.wasm-tools
    pkgs.wasmtime
    pkgs.wit-bindgen
    wasmLd
  ];

  mkWasip3ExampleTest = exampleName: let
    extraNames = exampleExtraHackagePackages.${exampleName} or [];
    packageFlags =
      pkgs.lib.concatMapStringsSep " " (name: "--package ${pkgs.lib.escapeShellArg name}") extraNames;
  in
    mkSourceCheck "aihc-wasip3-example-${exampleName}" (sources.exampleSrc exampleName pkgs) wasip3ExampleInputs ''
      set -euo pipefail
      export GHCRTS=-N1
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      export AIHC_WASM_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
      export AIHC_WASM_SYSROOT=${wasmSysroot}
      empty_stderr="$TMPDIR/empty-stderr"
      touch "$empty_stderr"
      package_flags=(${packageFlags})

      source="examples/${exampleName}/Main.hs"
      example_directory=$(dirname "$source")
      example_name=${pkgs.lib.escapeShellArg exampleName}
      expected_stdout="$example_directory/stdout"
      if [[ ! -f "$expected_stdout" ]]; then
        echo "Missing expected stdout for $source: $expected_stdout" >&2
        exit 1
      fi

      store=${exampleStoreFor "wasm32-wasip3" exampleName}
      ${pkgs.lib.concatMapStringsSep "\n" renderWasip3ExampleTest (wasip3CompilationModes exampleName)}

      touch "$out"
    '';

  wasip3ExampleCases =
    map (exampleName: {
      name = exampleName;
      path = mkWasip3ExampleTest exampleName;
    })
    exampleNames;

  # Every example gets one incremental WASI smoke test. Nix schedules these
  # derivations in parallel against the immutable shared library and runtime
  # artifacts. Whole-program linking has focused CLI coverage because it
  # intentionally recompiles the merged dependency bodies.
  wasip3ExampleTest = assert exampleNames != [];
    pkgs.linkFarm "aihc-wasip3-example-test" wasip3ExampleCases;
  # Compile every example for one target without linking. Each example gets a
  # relocatable bundle that `aihc link-exe`, or the C driver for the target,
  # turns into the executable on a host that has the linker but not the
  # compiler. The weekly cross-compilation workflow builds this on Linux and
  # links the bundles on macOS. A failing example records its status and log
  # instead of failing the derivation, so the consumer reports every example.
  crossExampleBundlesFor = target: let
    toolchain = exampleToolchainWith (crossSetupFor target) target;
    renderExample = exampleName: let
      extraNames = exampleExtraHackagePackages.${exampleName} or [];
      packageFlags =
        pkgs.lib.concatMapStringsSep " " (name: "--package ${pkgs.lib.escapeShellArg name}") extraNames;
    in ''
      example_name=${pkgs.lib.escapeShellArg exampleName}
      bundle="$out/$example_name"
      mkdir -p "$bundle"
      cp -R --no-preserve=mode "examples/$example_name" "$bundle/example"
      if (
        set -euo pipefail
        package_flags=(${packageFlags})
        ${mkExampleExtraInstall {
          inherit toolchain;
          targets = [target];
        }
        exampleName}
        timeout --foreground --kill-after=5s 300s ${aihcExe} build-exe "examples/$example_name/Main.hs" \
          --target ${target} \
          --gc semispace \
          --store "$store" \
          --build-root "$TMPDIR/.aihc-cache-$example_name" \
          "''${package_flags[@]}" \
          --no-link \
          --output "$bundle/link"
      ) > "$bundle/compile.log" 2>&1; then
        echo ok > "$bundle/status"
      else
        echo compile-failed > "$bundle/status"
        echo "Compiler failed for $example_name/${target}" >&2
      fi
    '';
  in
    pkgs.runCommand "aihc-cross-examples-${target}" {
      src = examplesSource;
      nativeBuildInputs = exampleTestInputs;
    } ''
      cd "$src"
      export GHCRTS=-N1
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      ${crossSetupFor target}
      mkdir -p "$out"
      echo ${target} > "$out/target"
      ${pkgs.lib.concatMapStrings renderExample exampleNames}
      # Every bundle carries its own copy of the library, entry, and runtime
      # archives. Hard-link identical inputs so an archive of the output
      # stays close to the size of one bundle.
      declare -A seen=()
      while IFS= read -r -d "" input; do
        digest=$(sha256sum "$input" | cut -d ' ' -f 1)
        if [[ -n "''${seen[$digest]:-}" ]]; then
          ln -f "''${seen[$digest]}" "$input"
        else
          seen[$digest]="$input"
        fi
      done < <(find "$out" -path '*/link/inputs/*' -type f -print0 | sort -z)
    '';
in {
  checks = {
    resolve-tests = resolveTests;
    tc-tests = tcTests;
    testing-tests = testingTests;
    aihc-tests = aihcTests;
    fmt-tests = fmtTests;
    haddock-tests = haddockTests;
    unicode-generated = unicodeGenerated;
    nix-lint = nixLint;
    nix-format = nixFormat;
    haskell-lint = haskellLint;
    haskell-format = haskellFormat;
    c-lint = cLint;
    c-format = cFormat;
    cabal-format = cabalFormat;
    core-libraries-install = coreLibrariesInstall;
    hackage-install-tests = hackageInstallTests;
    examples-tests = examplesTests;
    wasip3-example-test = wasip3ExampleTest;
  };
  packages = {
    cross-examples-apple-arm64 = crossExampleBundlesFor "apple-arm64";
  };
}
