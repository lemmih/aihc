{
  mkHsPkgs,
  mkWasiSysroot,
}: pkgs: let
  hsPkgs = mkHsPkgs pkgs;
in {
  default = pkgs.mkShell {
    buildInputs = [
      hsPkgs.ghc
      pkgs.cabal-install
      pkgs.ormolu
      pkgs.haskellPackages.cabal-gild
      pkgs.alejandra
      pkgs.hlint
      pkgs.clang-tools
      (pkgs.writeShellScriptBin "wasm-ld" ''
        exec ${pkgs.lld}/bin/wasm-ld "$@"
      '')
      pkgs.wasm-tools
      pkgs.wasmtime
      pkgs.wit-bindgen
      pkgs.zlib
      pkgs.python3Packages.mkdocs-material
    ];
    AIHC_WASM_CLANG = "${pkgs.llvmPackages.clang-unwrapped}/bin/clang";
    AIHC_WASM_SYSROOT = mkWasiSysroot pkgs;
  };
}
