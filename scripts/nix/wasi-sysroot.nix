# The WASI sysroot the wasm32-wasip3 target compiles and links against.
#
# Nixpkgs splits wasi-libc into its headers and its archives, and the compiler
# wants one directory holding both, the way a Homebrew or wasi-sdk
# installation already provides. wasi-libc also renamed its target directory
# from wasm32-wasi to wasm32-wasip1, and the pinned version can carry either
# name, so both are published here and one --target/--sysroot pair works
# whichever version nixpkgs holds.
pkgs: let
  wasilibc = pkgs.pkgsCross.wasi32.wasilibc;
in
  pkgs.runCommand "aihc-wasi-sysroot" {} ''
    mkdir -p "$out/include" "$out/lib"
    ln -s ${wasilibc.dev}/include/* "$out/include/"
    ln -s ${wasilibc}/lib/* "$out/lib/"
    for directory in include lib; do
      if [ ! -e "$out/$directory/wasm32-wasip1" ]; then
        ln -s wasm32-wasi "$out/$directory/wasm32-wasip1"
      fi
    done
    test -e "$out/include/wasm32-wasip1/stdlib.h"
    test -e "$out/lib/wasm32-wasip1/libc.a"
  ''
