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
former's package dependency. The store contains installed library interfaces,
whole-program bodies, and library archives for each target.
Each target has one installed entry per package name and version.
Runtime archives use keys for the target and garbage collector. An incomplete store is
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

## Artifact reuse

A package is either immutable or local, and the two never mix.

A Hackage release and a core library are immutable for a given compiler.
`aihc install` puts them into the store under `<store>/<target>/<name>-<version>-<fingerprint>`.
The fingerprint is a function of the plan alone: the package name and version, the compiler identity, the target, and the identities of the dependencies.
It reads no sources, so a consumer computes it without them, and nothing ever lists the store.
An existing directory is used as it is.
`--reinstall` builds the named package again and replaces its directory.
The manifest records the flags the entry was built with, and an install that asks for an output the entry lacks, such as `--keep-core`, must pass `--reinstall`.

A package installed from a local directory is mutable.
It builds in place under `<directory>/.aihc-target/<target>/<name>-<version>`, or under `--build-root`.
`--immutable` installs a local package into the store instead, as a core library is published.

In a build directory every unit keeps a stamp beside its artifacts: the digests of the inputs it was built from, the digests of the interfaces it wrote, and the size and modification time of each artifact file.
The inputs of a unit are its sources, the scope and type interfaces of the modules it imports, the instance facts of the units below it, and the instance digest of each dependency package it imports from.
A later build parses the sources, walks the units in dependency order, and reuses a unit whose recorded inputs equal the current digests and whose artifacts are the recorded files.
Everything else is rebuilt in place.
No interface is encoded to learn its digest: source digests come from parsing, and artifact digests are taken from the bytes as they are written.
Each package writes `digests.json` next to its manifest, from which consumers take the digests of its interfaces.

`aihc build-exe` resolves its `--package` constraints through the same plan.
The plan reads the Cabal files of the packages, from the Hackage download cache or from `--workspace DIR`, which holds a package source under `DIR/NAME`.
A package that is absent from the store is built.
The modules of the executable build under `.aihc-target` in the working directory, or under `--build-root`, with the same stamps as a local package.

The Cabal build hook uses the current Git commit hash as the compiler identity.
If Git or a commit is absent, the compiler identity is empty.
Uncommitted compiler changes do not change this identity.
The compiled program contains this identity as a pure constant.
Host compiler and archiver identities use hashes of their resolved paths.
After a tool update at the same path, users must remove obsolete store entries.
