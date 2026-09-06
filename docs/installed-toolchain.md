# Installed runtimes and libraries

`aihc compile` consumes prepared runtime archives and installed library
artifacts. It does not compile either dependency from source while building an
application.

Prepare every runtime variant that applications will select:

```console
aihc prepare-runtime --target llvm --gc semispace --store "$AIHC_STORE"
aihc prepare-runtime --target wasm32-wasip3 --gc semispace --store "$AIHC_STORE"
```

Then install the packages needed by the applications. Package dependencies are
installed recursively, and Cabal metadata selects the library modules. The
frontend is compiled once even when several targets are requested:

```console
aihc install core-libs/aihc-base \
  --offline \
  --store "$AIHC_STORE" \
  --target llvm \
  --target wasm32-wasip3
```

The package argument is a local Cabal package directory. When no such directory
exists, it is read as a Hackage package name with an optional version and the
sources are fetched from Hackage; without a version the preferred version on
Hackage is used:

```console
aihc install nats --store "$AIHC_STORE" --target llvm
aihc install nats-1.1.1 --store "$AIHC_STORE" --target llvm
```

Application compilation only selects the installed store, target, and runtime
variant:

```console
aihc compile Main.hs \
  --store "$AIHC_STORE" \
  --target llvm \
  --gc semispace \
  --output program
```

There is no special core-library installation mechanism: `aihc-base` and
`aihc-prim` are ordinary packages, with the latter installed through the
former's package dependency. The store contains content-addressed library
interfaces and whole-program bodies, target-specific library archives, and
runtime archives keyed by target and garbage collector. An incomplete store is
an error; application compilation never fills in missing artifacts by rebuilding
source dependencies.

## Linking on another host

`aihc build-exe --no-link` stops before the link and writes a bundle
directory instead of an executable. The bundle holds a copy of every link
input, so it is complete on its own: the module objects, the C objects and
archives of the installed packages, and the entry and runtime archives. A
`link.json` manifest lists them in link order with paths relative to the
bundle.

```console
aihc build-exe Main.hs \
  --store "$AIHC_STORE" \
  --target apple-arm64 \
  --gc semispace \
  --no-link \
  --output program-bundle
```

On Linux, compiling for `apple-arm64` needs the macOS SDK headers for the C
runtime and for package C sources. `AIHC_APPLE_SDK` names the SDK root, which
the compiler passes as `-isysroot`, and `AIHC_APPLE_CLANG` selects the Clang
executable, which under Nix should be the unwrapped one because the wrapper
adds Linux-only arguments. The Nix package `cross-examples-apple-arm64` sets
both, fetching the SDK the way nixpkgs does for its Darwin toolchain.

`aihc link-exe` finishes the bundle on a host that has the linker for the
target, such as a Mac for `apple-arm64` objects compiled on Linux:

```console
aihc link-exe program-bundle --output program
```

The manifest is plain JSON, so a host without the compiler can also run the
link through the C driver of the target directly. The weekly cross-compilation
workflow does this: `nix build .#cross-examples-apple-arm64` compiles every
example to a bundle on Linux, and `scripts/link-and-run-example-bundles.sh`
links and runs the bundles on macOS.
