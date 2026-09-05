# WASI P3 backend

The `wasm32-wasip3` target lowers GC-GRIN to Lir and emits LLVM MC's
WebAssembly assembly syntax with `Aihc.Wasm.Lir` (see `docs/lir.md`). This is
WebAssembly machine code: the backend selects Wasm instructions, locals,
structured control flow, data objects, and the runtime ABI directly. Clang's
integrated Wasm assembler only serializes those instructions and records
relocations; generated Haskell code does not pass through C or LLVM IR.

The build pipeline uses temporary linker inputs:

```text
dependency GC-GRIN -> Lir -> WebAssembly assembly -> cached dependency objects
main GC-GRIN       -> Lir -> WebAssembly assembly -> program.o
Lir entry unit     -> WebAssembly assembly -> entry object
C runtime + P3 IO backend       -> runtime objects
WIT C bindings                  -> binding object
all objects -> wasm-ld -> core module -> wasm-tools -> component
```

The resulting output is one WebAssembly component. The object files and the
intermediate core module are removed after linking.

The driver invokes the standard LLVM tools directly: `clang
--target=wasm32-wasip1`, `wasm-ld`, `wasm-tools`, and `wit-bindgen`. They may
come from any LLVM/WASI installation on `PATH`; no `wasm32-clang` wrapper is
required. `AIHC_WASM_CLANG` can select another Clang executable when a host
toolchain wrapper is not cross-target safe. The Nix development environment
uses that override to select its unwrapped LLVM Clang.

## The WASI sysroot

The runtime allocates, copies memory, and aborts through libc like every
other target, so the target needs a WASI sysroot. Install one with
`brew install wasi-libc` or from a
[wasi-sdk release](https://github.com/WebAssembly/wasi-sdk/releases), and set
`AIHC_WASM_SYSROOT` when it sits outside a standard prefix. The compiler
reports the prefixes it searched when it finds none.

The C sources of the runtime compile against the headers of that sysroot and
`wasm-ld` takes its `libc.a` after every other input, so the linker draws the
allocator and the memory routines from it and nothing else. The build stays
`-nostdlib`: the archive is an explicit input and the driver never adds a
startup object of its own.

Whatever the runtime takes from libc has to compute without the host. The
component model cannot describe a WASI preview 1 import, and the P3 pipeline
encodes the linked module with no adapter, so a libc function that calls the
host, such as one of the stdio, exit, or clock families, fails when
`wasm-tools component new` cannot resolve `wasi_snapshot_preview1`. The IO the
runtime does perform goes through the P3 bindings below instead.

## Runtime ABI

Generated functions have the Lir signatures of the lowering: the machine, the
GRIN parameters, and no results. Every CPS transfer is a `return_call`, so the
whole program runs inside the call that started it. Each Lir value is a
WebAssembly local; values reach linear memory only at the boundaries that
need an address, such as the live-root vector of a collection safepoint on the
shadow stack.

The P3 driver owns the IO loop. The Lir entry unit exports
`aihc_lir_program_start`, which creates the machine and evaluates the entry,
and `aihc_lir_program_resume`, which continues a scheduler resumption after an
IO request completes. Both return when the machine halts or when every green
thread waits for IO, and both report which of the two happened. The driver
returns `WAIT` to the host in the second case and resumes the program from its
callback.

Runtime info tables are ordinary relocatable data objects with 4-byte words.
Function addresses in those tables become Wasm table indices when `wasm-ld`
links the program and runtime. Heap pointers remain 32-bit Wasm addresses
stored in the shared 8-byte slot type used by the other backends.

## Asynchronous stdout

The initial P3 IO backend implements stdout writes with
`wasi:cli/stdout@0.3.0`. It creates a `stream<u8>`, supplies its readable end to
`write-via-stream`, and incrementally writes the AIHC IO buffer through the
writable end. When the stream or result future blocks, the exported async
`wasi:cli/run@0.3.0` callback returns `WAIT(waitable-set)`. A later callback
finishes the request, makes its green thread runnable, and resumes the
program through `aihc_lir_program_resume`.

The `System.IO` `stdout` handle uses this path, including its `MVar`-serialized
handle state and native-width `Int` FFI results. The current WIT world does not
import stdin, stderr, or filesystem interfaces. Those fixed handles still
exist, but unsupported operations and `openBinaryFile` report an IO error; an
uncaught `IOException` traps because the component has no synchronous error
stream.

## Incremental compilation

Incremental compilation is the default. Each dependency SCC is compiled with
the complete linked program set and produces a relocatable Wasm object.
Objects are cached in target-specific library archives. Static objects and
info tables are data objects with link-time addresses, so no module needs an
initializer.

`--whole-program` remains available. It merges reachable dependency Core before
GRIN lowering and emits one generated-code object. Both modes compile the C
runtime and WIT bindings only at the final link and produce one component.
