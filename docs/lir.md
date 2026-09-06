# Lir

Lir is the low-level intermediate language of AIHC. It sits between GC-GRIN and
the machine backends. The intended pipeline is:

```text
Haskell -> System FC -> GRIN -> CPS-GRIN -> GC-GRIN -> Lir -> AMD64 / ARM64 / WebAssembly / LLVM
```

Lir has one purpose. It gives every backend the same simple input. The runtime
system is moving to Lir a unit at a time; the section "Runtime units" lists the
units that have moved. After the move, the optimizer sees one program without a
boundary between user code and runtime code.

This document is the specification. The implementation lives in
`bin/aihc/compiler/lir`. The specification and the implementation change
together.

Every target consumes Lir. The module `Aihc.Lir.Lower` lowers GC-GRIN to Lir.
The backends are `Aihc.Arm64.Lir` (Mach-O objects for Apple ARM64),
`Aihc.Amd64.Lir` (ELF objects for Linux AMD64), `Aihc.Llvm.Lir` (textual
LLVM IR), and `Aihc.Wasm.Lir` (WebAssembly assembly for WASI P3). The module
`Aihc.Cli.Backend` selects the backend of a target for `aihc install`,
`aihc prepare-runtime`, and `aihc build-exe`. The sections "Lowering from
GC-GRIN" and "Backends" describe them.

## Design rules

- Lir is a control-flow graph in static single assignment form.
- Blocks have parameters. Lir has no phi instructions.
- Every operation has a defined result. Lir has no undefined behavior. An
  operation that has no valid result traps.
- Lir has no garbage collector and no exception handler. GRIN makes both
  explicit before it produces Lir.
- Lir has no hidden state. Runtime registers are declared globals.
- The text format and the in-memory representation carry the same information.
  The pretty-printer and the parser round-trip every module.

## Lexical structure

A `;` starts a comment. The comment ends at the end of the line. White space and
comments separate tokens. Layout has no meaning.

Names have three forms:

| Form | Example | Meaning |
| --- | --- | --- |
| `%name` | `%acc` | A value inside a function |
| `@name` | `@sum` | A symbol: a function, a global, or a data object |
| `name` | `loop` | A block label inside a function |

A bare name uses the characters `A-Z`, `a-z`, `0-9`, `_`, `.`, and `$`. A
quoted name is a string literal after the sigil, for example `@"Data.List\u{0}map"`.
A quoted name can contain every character.

A string literal is enclosed in double quotes. The escapes are `\\`, `\"`, `\n`,
`\r`, `\t`, `\0`, `\xHH` with two hexadecimal digits, and `\u{H...}` with one
to six hexadecimal digits. In a name or a trap message, `\xHH` is the character
with that code. In a `bytes` field, `\xHH` is one byte and every other
character contributes its UTF-8 encoding. The pretty-printer emits a bare name
when the name permits it. Otherwise it emits a quoted name. A bare label also
starts with a letter or an underscore, so a label is never confused with a
literal.

Integer literals are decimal with an optional sign. Float literals contain a
decimal point or an exponent, for example `1.5` and `1.0e-3`. The literals
`inf`, `-inf`, and `nan` are also float literals. The literal `null` is the
`ptr` or the `code` value with address zero.

## Types

| Type | Meaning |
| --- | --- |
| `i1` | A boolean. The value is `0` or `1`. |
| `i8`, `i16`, `i32`, `i64` | An integer with the given number of bits. |
| `f32`, `f64` | An IEEE 754 binary float. |
| `ptr` | The address of data. Its size is the target word size. |
| `code` | The address of a function. Its size is the target word size. |

Integer types have no sign. Each operation names the sign it uses. The types
`i8` to `i64` are the integer types. The type `i1` is not an integer type. It
supports only the operations that this document lists for it.

The type `ptr` is opaque. Integer operations do not accept it. `ptr.to_int`
gives the address as an `i64`. On a 32-bit target the high 32 bits are zero.
`ptr.from_int` makes a pointer from an `i64`. On a 32-bit target it discards
the high 32 bits.

The type `code` is the type of a function symbol. A `code` value supports only
`eq`, `ne`, `select`, `load`, `store`, `call.indirect`, and `tailcall.indirect`.
No operation converts between `code` and another type, and no operation
computes a `code` value from an integer. This keeps `code` valid on targets
where a function address is not a memory address. On WebAssembly a `code` value
is an index into the function table.

A literal `n` fits a type `iN` when `n` is in the signed range or the unsigned
range of `iN`. A literal for `i1` is `0` or `1`. A literal for `ptr` is `null`
or the symbol of a data object. A literal for `code` is `null` or the symbol of
a function. A literal for `f32` or `f64` is a float literal or an integer
literal. A global has no address, so its symbol is not a literal.

## Module

A module is a sequence of items. Item order is not significant, except that the
pretty-printer preserves it.

```text
item ::= function | extern-function | global | data | extern-data
```

Every symbol is defined or declared at most once in a module.

### Functions

```text
function ::= "export"? "func" symbol "(" parameters ")" results? cc? "{" block+ "}"
parameters ::= (value ":" type ("," value ":" type)*)?
results ::= "->" type | "->" "(" type ("," type)* ")"
cc ::= "cc" ("aihc" | "c")
```

The default calling convention is `aihc`. The `aihc` convention permits every
arity and every result count. Tail calls are guaranteed. An `aihc` function
preserves no register: a call clobbers them all. The `c` convention is the
platform C convention. Use it for the host boundary. A function without
`export` is internal to the module.

The first block is the entry block. The entry block has no parameters and no
predecessors. The function parameters are visible in every block.

```text
extern-function ::= "extern" "func" symbol "(" (type ("," type)*)? ")" results? cc?
```

An extern function is defined in another module or in the host.

### Globals

```text
global ::= "global" symbol ":" type "pinned"?
```

A global is one mutable cell of the given type. `global.get` and `global.set`
are the only operations that access it. A global has no address. The initial
value of a global is zero. A `pinned` global tells the backend to keep the cell
in a register when the target permits it. The heap pointer and the heap limit
are pinned globals.

### Data

```text
data ::= "export"? "data" "mut"? symbol "align" integer "=" "{" field ("," field)* "}"
field ::= int-type integer
        | float-type float
        | "word" integer
        | "ptr" symbol (("+" | "-") integer)?
        | "ptr" "null"
        | "code" symbol
        | "code" "null"
        | "bytes" string
        | "zero" integer
extern-data ::= "extern" "data" symbol
```

A data object is a sequence of bytes in memory with a fixed address. Its
alignment is a power of two. The fields are stored in order without padding.
Integers and floats are little-endian. A `ptr` field stores the address of a
data object plus an addend. The addend gives tagged headers a direct encoding.
A `code` field stores the address of a function. The fields `ptr null` and
`code null` store a word of zero bytes; unlike `zero`, their size follows the
target word size. A `word` field stores an integer in the target word size, so
one hand-written module describes a word-shaped record on a 64-bit and on a
32-bit target alike. Its value fits 32 bits, signed or unsigned, so no target
truncates it. A `bytes` field stores the UTF-8 encoding of the string. A
`zero` field stores the given number of zero bytes.

A data object without `mut` is read-only. A store to a read-only data object
traps.

### Info tables

An info table describes one kind of heap object. The header of a heap object
is the address of its info table. GC-GRIN emits one info table per object kind
as a read-only data object, so every backend receives the same layout and emits
it as bytes. No backend computes an info table of its own.

Every field of an info table is one word wide. A pointer field is `ptr`, a
code field is `code`, and a count or a kind is an integer of the word width:
`i64` on a 64-bit target and `i32` on a 32-bit target. The lowering knows its
target and emits that type; a hand-written module writes `word` instead and
suits every target. Field `k` starts at offset `k` words, and the table is
aligned to the word size. A field without a value is `ptr null`, `code null`,
or `0`. The fields are, in order:

| Field | Type | Meaning |
| --- | --- | --- |
| `identity` | `ptr` | The saturated constructor table of a constructor. Case code compares this field. A closure or a thunk stores the `code` of its function here, and the heap snapshot tool maps that address to a name. |
| `entry` | `code` | The portable entry. Reserved: the lowering stores null until the runtime moves to Lir. |
| `field_count` | integer | The number of payload words. |
| `remaining_arity` | integer | The number of arguments the object still requires. |
| `field_is_pointer` | `ptr` | A `bytes` data object with one byte per payload word: `1` for a managed pointer, `0` otherwise. Null when `field_count` is `0`. |
| `next` | `ptr` | The table of the next application stage. Null for the last stage. |
| `backend_entry` | `code` | The direct entry. Null when the object cannot be entered. |
| `frame_kind` | integer | The continuation frame kind for stack unwinding. |
| `object_kind` | integer | Node, closure, thunk, partial constructor, or a runtime object kind. |
| `srt` | `ptr` | The static reference table, or null. |

The `backend_entry` field has the signature `(ptr, ptr, ptr, T...) -> ()`
with the machine, the object, the continuation, and the supplied values. The
types `T...` are the Lir types of the supplied values, so a call site with
`n` supplied values states a signature with `n` value parameters. A
continuation object ignores the continuation parameter. The lowering
generates one function with this signature for each enterable object. That
function loads the stored fields, takes the supplied values as parameters,
and tail-calls the code of the object.

The runtime's `AihcInfo` structure has the layout of this section on every
target: its counts and kinds are `uintptr_t`. On WebAssembly `call.indirect`
checks the type of the callee, and the lowering states the signature with the
types of the supplied values at every call site, so the same enter stubs work
there.

## Functions and blocks

```text
block ::= label ("(" parameters ")")? ":" instruction* terminator
instruction ::= (value ("," value)* "=")? operation
```

Every value is defined once. A use of a value is valid when its definition
dominates the use. A block parameter is defined at the start of the block.

The last instruction of a block is a terminator. No other instruction is a
terminator.

## Operands

An operand is a value or a literal. The type of an operand comes from the
operation. Block arguments take their types from the block parameters.

## Operations

The notation `T` is a type. `iN` is an integer type. `fN` is a float type. The
operation trap conditions are the complete list. An operation with no trap
condition never traps.

### Integer arithmetic

| Operation | Result | Semantics |
| --- | --- | --- |
| `add iN %a, %b` | `iN` | Wrapping addition. |
| `sub iN %a, %b` | `iN` | Wrapping subtraction. |
| `mul iN %a, %b` | `iN` | Wrapping multiplication. |
| `div.s iN %a, %b` | `iN` | Signed division. Rounds toward zero. Traps when `%b` is zero. Traps when the result does not fit. |
| `div.u iN %a, %b` | `iN` | Unsigned division. Traps when `%b` is zero. |
| `rem.s iN %a, %b` | `iN` | Signed remainder. The sign follows `%a`. Traps when `%b` is zero. |
| `rem.u iN %a, %b` | `iN` | Unsigned remainder. Traps when `%b` is zero. |
| `and iN %a, %b` | `iN` | Bitwise and. Also accepts `i1`. |
| `or iN %a, %b` | `iN` | Bitwise or. Also accepts `i1`. |
| `xor iN %a, %b` | `iN` | Bitwise exclusive or. Also accepts `i1`. |
| `shl iN %a, %b` | `iN` | Shift left. The count is `%b` modulo `N`. |
| `shr.s iN %a, %b` | `iN` | Arithmetic shift right. The count is `%b` modulo `N`. |
| `shr.u iN %a, %b` | `iN` | Logical shift right. The count is `%b` modulo `N`. |
| `clz iN %a` | `iN` | The number of leading zero bits. `N` when `%a` is zero. |
| `ctz iN %a` | `iN` | The number of trailing zero bits. `N` when `%a` is zero. |
| `popcount iN %a` | `iN` | The number of one bits. |
| `mul.wide.s iN %a, %b` | `iN, iN` | Signed full multiplication. The results are the low and the high half. |
| `mul.wide.u iN %a, %b` | `iN, iN` | Unsigned full multiplication. The results are the low and the high half. |
| `add.carry iN %a, %b` | `iN, i1` | Wrapping addition and the unsigned carry. |
| `sub.borrow iN %a, %b` | `iN, i1` | Wrapping subtraction and the unsigned borrow. |

### Comparison

| Operation | Result | Semantics |
| --- | --- | --- |
| `eq T %a, %b` | `i1` | Equal. `T` is any type, including `code`. Float comparison is IEEE 754. |
| `ne T %a, %b` | `i1` | Not equal. `T` is any type. |
| `lt.s iN %a, %b` | `i1` | Signed less than. |
| `lt.u T %a, %b` | `i1` | Unsigned less than. `T` is an integer type or `ptr`. |
| `le.s iN %a, %b` | `i1` | Signed less than or equal. |
| `le.u T %a, %b` | `i1` | Unsigned less than or equal. `T` is an integer type or `ptr`. |
| `gt.s iN %a, %b` | `i1` | Signed greater than. |
| `gt.u T %a, %b` | `i1` | Unsigned greater than. `T` is an integer type or `ptr`. |
| `ge.s iN %a, %b` | `i1` | Signed greater than or equal. |
| `ge.u T %a, %b` | `i1` | Unsigned greater than or equal. `T` is an integer type or `ptr`. |
| `flt fN %a, %b` | `i1` | Ordered less than. False when an operand is NaN. |
| `fle fN %a, %b` | `i1` | Ordered less than or equal. |
| `fgt fN %a, %b` | `i1` | Ordered greater than. |
| `fge fN %a, %b` | `i1` | Ordered greater than or equal. |

`eq` and `ne` on `fN` follow IEEE 754. NaN is not equal to any value.

### Float arithmetic

| Operation | Result | Semantics |
| --- | --- | --- |
| `fadd fN %a, %b` | `fN` | IEEE 754 addition. |
| `fsub fN %a, %b` | `fN` | IEEE 754 subtraction. |
| `fmul fN %a, %b` | `fN` | IEEE 754 multiplication. |
| `fdiv fN %a, %b` | `fN` | IEEE 754 division. Division by zero gives an infinity or NaN. |
| `fneg fN %a` | `fN` | Negation. |
| `fabs fN %a` | `fN` | Absolute value. |
| `fsqrt fN %a` | `fN` | Square root. |

### Conversion

| Operation | Result | Semantics |
| --- | --- | --- |
| `sext iN %a to iM` | `iM` | Sign extension. `M` is greater than `N`. Also accepts `i1` as `iN`. |
| `zext iN %a to iM` | `iM` | Zero extension. `M` is greater than `N`. Also accepts `i1` as `iN`. |
| `trunc iN %a to iM` | `iM` | Truncation. `M` is less than `N`. Also accepts `i1` as `iM`. |
| `itof.s iN %a to fM` | `fM` | Signed integer to float. Rounds to nearest even. |
| `itof.u iN %a to fM` | `fM` | Unsigned integer to float. Rounds to nearest even. |
| `ftoi.s fN %a to iM` | `iM` | Float to signed integer. Rounds toward zero. Traps on NaN and out of range. |
| `ftoi.u fN %a to iM` | `iM` | Float to unsigned integer. Rounds toward zero. Traps on NaN and out of range. |
| `fpext f32 %a to f64` | `f64` | Widen a float. |
| `fptrunc f64 %a to f32` | `f32` | Narrow a float. Rounds to nearest even. |
| `bitcast T %a to U` | `U` | Reinterpret the bits. `T` and `U` have the same width. One is a float type and one is an integer type. |
| `ptr.to_int %p` | `i64` | The address of a pointer. |
| `ptr.from_int %i` | `ptr` | The pointer with the given address. |

### Selection

| Operation | Result | Semantics |
| --- | --- | --- |
| `select T %c, %a, %b` | `T` | `%a` when `%c` is `1`. `%b` otherwise. `%c` is `i1`. `T` is any type. |

### Memory

```text
address ::= "[" value (("+" | "-") integer)? "]"
```

The base of an address is a `ptr` value. The offset is a constant.

| Operation | Result | Semantics |
| --- | --- | --- |
| `load T address align A` | `T` | Read a `T` from the address. Traps when the address is not mapped. Traps when the address is not a multiple of `A`. |
| `store T %v, address align A` | none | Write `%v` to the address. Traps when the address is not mapped, is read-only, or is not a multiple of `A`. |
| `ptr.add %p, %i` | `ptr` | Add an `i64` to a pointer. The addition wraps at the target word size. |
| `stack.alloc N align A` | `ptr` | Reserve `N` bytes of stack memory. The memory is zero. It lives until the function returns. Only the entry block may contain this operation. |

`A` is a power of two. `T` is `i1` only for `load` and `store` of one byte. `T`
may be `code`. Loading a `code` value from bytes that are not the address of a
function gives a value that traps in `call.indirect`.

### Globals

| Operation | Result | Semantics |
| --- | --- | --- |
| `global.get @g` | the type of `@g` | Read the global. |
| `global.set @g, %v` | none | Write the global. |

### Calls

| Operation | Result | Semantics |
| --- | --- | --- |
| `call @f(args)` | the results of `@f` | Call a function or an extern function. |
| `call.indirect %p(args) : signature` | the results of the signature | Call the code at `%p`. `%p` is `code`. Traps when `%p` is not a function with the same signature. |

```text
signature ::= "(" (type ("," type)*)? ")" results? cc?
```

The argument types are the parameter types of the target. A call with a `c`
convention target may have at most one result.

## Terminators

```text
target ::= label ("(" (operand ("," operand)*)? ")")?
```

The arguments of a target match the parameters of the block.

| Terminator | Semantics |
| --- | --- |
| `jump target` | Continue at the target. |
| `br %c, target1, target2` | Continue at `target1` when `%c` is `1`. Otherwise continue at `target2`. `%c` is `i1`. |
| `switch iN %v { case -> target ... default -> target }` | Continue at the target of the case that equals `%v`. Without a match, continue at the default target. Without a default, trap. |
| `return operands` | Return the operands. Their types are the result types of the function. |
| `tailcall @f(args)` | Replace the current activation by a call of `@f`. The result types and the calling convention of `@f` equal those of the current function. |
| `tailcall.indirect %p(args) : signature` | The indirect form of `tailcall`. |
| `trap "message"` | Stop the program with the message. |

Switch cases are distinct literals that fit `iN`.

## Traps

A trap stops the program. The message of a trap is one of these strings or the
message of a `trap` terminator:

| Message | Cause |
| --- | --- |
| `integer division by zero` | `div.s`, `div.u`, `rem.s`, or `rem.u` with a zero divisor. |
| `integer overflow` | `div.s` of the minimum value by minus one. |
| `invalid float to integer conversion` | `ftoi.s` or `ftoi.u` of NaN or of an out of range value. |
| `memory access out of bounds` | A load or a store outside mapped memory. |
| `misaligned memory access` | A load or a store with an address that is not a multiple of the alignment. |
| `store to read-only data` | A store to a data object without `mut`. |
| `indirect call to a non-function` | `call.indirect` of a `code` value that is not the address of a function, for example `null`. |
| `indirect call signature mismatch` | `call.indirect` of a function with a different signature. |
| `switch without a matching case` | A `switch` without a default and without a matching case. |
| `stack overflow` | The stack memory is exhausted. |

## Lint

The linter checks every rule of this document that the parser cannot check. A
module passes the linter before it reaches a backend. The linter reports each
error as `@symbol/block: message`. It omits the block, or the symbol and the
block, when they do not apply.

The test fixtures in `bin/aihc/compiler/lir/test/Test/Fixtures/lir/lint` give
the exact text of every error. Each fixture is a Lir module. Each header comment
`; error: <text>` is one expected error. The test asserts the complete list in
order.

## Interpreter

The interpreter is the reference implementation of this document. It executes a
module from a named function and reports the results or the trap. Memory is a
flat address space. Data objects, the stack, and code addresses have distinct
regions. Code addresses are not readable, and a `ptr.from_int` of a code
address is a pointer that traps on `load` and `store`. The interpreter uses a
64-bit word size. It cannot call an extern function.

The interpreter renders results with their declared types. An `iN` result is a
signed decimal. An `i1` result is `0` or `1`. A float result uses the Haskell
`show` format. A `ptr` or a `code` result is a hexadecimal address.

The test fixtures in `bin/aihc/compiler/lir/test/Test/Fixtures/lir/eval` are
Lir modules with a function `@main` without parameters. The header comment
`; expect: <results>` gives the rendered results separated by `, `. The header
comment `; expect-trap: <message>` gives the trap message instead. Every
fixture also passes the linter and the pretty-printer round-trip.

## Lowering from GC-GRIN

`Aihc.Lir.Lower` produces one Lir module for one GC-GRIN program. Every GRIN
function becomes a Lir function with the `aihc` convention and no results.
The first parameter is the machine. The other parameters are the GRIN
parameters in order. A GRIN value with a pointer representation or an address
representation becomes `ptr`. Every other GRIN value becomes `i64`, and a
float travels as its bit pattern like in the native runtime ABI.

The lowering takes a `LowerTarget`: the word size and the host kind. Heap
objects have 8-byte slots on every target, so a pointer that lives in a slot
travels through `i64` on a 32-bit target and the high bytes of the slot are
zero. The word size decides the layout of the info tables, the static
reference tables, and the resume records of the scheduler. The targets are
`posixTarget64` for Apple ARM64, Linux AMD64, and LLVM, and `wasip3Target`
for WebAssembly.

The lowering keeps the control model of CPS-GRIN:

- A direct call is a `tailcall`.
- A direct expression is a sequence of instructions. The integer, float,
  address, and memory primitives become Lir operations. A primitive that needs
  the heap, the collector, the scheduler, or the host becomes a `call` of an
  extern C function.
- A case on a pointer loads the header and the `identity` field and compares
  it with the constructor tables. A case on a scalar is a `switch`.
- A heap reservation stores the live roots in a `stack.alloc` array, calls
  `aihc_ensure_heap`, and reloads the relocated roots.
- Evaluation, application, continuation, and scheduler resumption go through
  shared functions that the lowering generates into every module that uses
  them. The functions `aihc_lir_continue_*` and `aihc_lir_apply_*` exist per
  shape of the supplied values.
- The executable entry unit defines the top, final, update, and thread done
  continuations and the exit function. On a POSIX host it defines `main`,
  which starts the machine and returns when the exit function returns. On
  WASI P3 it exports `aihc_lir_program_start` and `aihc_lir_program_resume`
  for the C driver, which owns the IO loop: a null scheduler resumption
  means that every thread waits for IO, and the resume helper returns to the
  driver. Both functions return one when the exit function has recorded
  that the machine halted.

The lowering emits the info tables, the enter stubs, the static objects, the
static reference tables, and the address literals as data objects. Static
objects are exported and mutable. Info tables are read-only. The collector
finds static objects by address, so a Lir module needs no root section and
both collectors work with this pipeline.

## Runtime units

A runtime unit is a `.lir` file in `bin/aihc/compiler/native/runtime` that
`aihc prepare-runtime` parses, lints, and compiles with the backend of the
target. Its object joins the C objects in the runtime archive, so a runtime
function written in Lir and one written in C call each other through the same
`c` convention and the same symbol names.

The archive is what a program links. `Aihc.Cli.Runtime.buildRuntimeArchive`
builds one, and a test harness that needs its own runtime — an instrumented
one, or one with a smaller semispace — calls it with the extra C arguments
instead of naming the runtime sources. Moving a unit from C to Lir then
changes no test. A link places the archive after the objects that reference
it.

The units are:

- `aihc_array.lir` holds the info table of a boxed array and the functions
  `aihc_array_new`, `aihc_array_index`, `aihc_array_write`, and
  `aihc_array_same`. The collector keeps `aihc_array_length` and
  `aihc_array_elements` in C, and the unit calls `aihc_array_length` for the
  object-kind check.
- `aihc_mutvar.lir` holds the `MutVar#` primitives. A mutable reference is a
  boxed array of one element, so every one of them calls the array unit.
- `aihc_stable_name.lir` holds the stable-name table: the lookup, the
  allocation, and the layout of one entry. The list head and the hash counter
  are machine fields whose offsets follow the target word size, so C keeps
  `aihc_stable_names` and `aihc_stable_name_take_hash` as accessors and the
  collector still walks the list itself.
- `aihc_byte_array.lir` holds the byte arrays. The collector never traces one,
  so this unit owns the whole layout and the C runtime keeps no description of
  it. Bulk moves call `aihc_memory_copy` and `aihc_memory_move`, which are
  `memcpy` and `memmove` behind a signature that states its length as an
  `i64`.

A unit reaches the C runtime only through functions, never through the fields
of a C structure, unless those fields sit one eight-byte slot apart on every
target and `aihc_runtime.c` asserts it. `AihcStableName` is the one record
with that shape today.

A unit is one file for every target, so it states no word size of its own. A
heap slot is eight bytes everywhere, so a header pointer travels through
`ptr.to_int` and `ptr.from_int` and a payload offset is a constant. A
word-shaped record uses `word` fields, which follow the target word size.

## Register allocation

`Aihc.Lir.RegAlloc` assigns registers to the values of one function. It is
target-independent: a backend describes the registers it is willing to give
away and receives, for each value, either one of them or the verdict that the
value stays in a frame slot. The AArch64 and the AMD64 backends share it.

The registers come in two classes. A volatile register is clobbered by every
call and costs nothing to use. A preserved register survives a C call,
because the C callee saves it, and is clobbered by an aihc call, because an
aihc function saves nothing. So a value that lives across a C call takes a
preserved register, a value that lives across an aihc call goes to a frame
slot, and everything else takes whatever is free. That is the whole of the
interaction between calls and registers: no interval is ever split, and no
register is ever pre-colored. Under the C convention a preserved register
costs the function a save and a restore, so there a value takes one only once
the function touches it more often than it has exits plus the one save, with
a touch inside a loop counting for a power of ten per enclosing loop. Under
the aihc convention a preserved register is free.

A hint is a register the scan tries first. Parameters, call arguments, call
results, and returned values are hinted with the register the convention
puts them in. The argument of a jump and the block parameter it reaches are
partners: each prefers the register the other already has, and failing that
the register the other was hinted with. Last, a result prefers the register
of an operand of its own instruction, which is free exactly when the operand
dies there; on a two-operand machine that is the difference between one
instruction and two. A hint that is not free at the time is dropped, so
hints cost nothing in correctness and buy most of the moves that a
convention would otherwise need. The value an instruction consumes may hand
its register to the value the instruction defines, which every instruction
a backend selects has to tolerate: it reads every operand before it writes a
result, or computes in a scratch register first.

The allocator numbers the blocks in the order the function states them,
computes live-in and live-out sets, and gives each value one interval from the
lowest to the highest position at which it is live. The interval has no holes
and the allocator never splits one, so a value that dies and revives inside
its span keeps its register throughout. That costs registers on a wide
function and buys independence from the block order: the result is correct
whatever order the blocks arrive in and whatever the loops look like. The scan
walks the intervals in order of their start, hands out the first free
acceptable register, hints first and then the pool in preference order, and
when nothing is free sends the acceptable interval that reaches furthest to
a frame slot.

## Backends

Every backend lints the module first. No backend checks the alignment of a memory access, a store to read-only
data, or the signature of an indirect call. A misaligned access gives the result of the hardware, and a store to
read-only data is a memory fault. Every backend checks an indirect call of
`null`.

The fixtures in `bin/aihc/compiler/lir/test/Test/Fixtures/lir/asm` are Lir
modules with one companion file per native backend holding the assembly that
backend produces. They are small and aimed at the register allocator, so a
change of allocation or of instruction selection reads as a diff of real code
rather than as a change of some object bytes. Run the suite with
`AIHC_ACCEPT_ASM=1` to rewrite the companion files.

### AArch64

`Aihc.Arm64.Lir` assembles the module with the direct Mach-O writer:

- The allocator hands out `x0` to `x13` as volatile registers and `x19` to
  `x28` as preserved ones; `x14` to `x17` are the scratch registers of
  instruction selection. A value the allocator spills lives in an 8-byte
  frame slot. Instruction selection reads a register operand in place, loads
  a slot or a literal into a scratch register, folds a small literal into
  the immediate of `add`, `sub`, and `cmp`, and writes the result straight
  into its home. A comparison that only the branch of its block reads is not
  materialized: the block compares and branches on the flags, or on the
  register itself against zero.
- The arguments of a call, the values of a return, and the arguments of a
  jump are one parallel move each: every source is read before the
  destination it lives in is written, a cycle is broken through a scratch
  register, and a value that is already where the convention wants it costs
  nothing.
- The `aihc` convention passes the first eight arguments in `x0` to `x7` and
  the rest in a 16-byte aligned block on the stack. The callee pops that
  block. Results come back in `x0` to `x7`. An aihc function preserves no
  register, so one that calls nothing and spills nothing has no frame: it
  leaves the stack pointer where it found it. A function with a frame saves
  the frame pointer pair and keeps its slots below it. A tail call writes
  its outgoing block in place of the incoming one when it is no larger,
  moves the stack pointer down to make room when it is larger and there is
  no frame, and otherwise builds the block below the frame and copies it up
  once the frame is gone. The stack does not grow.
- The `c` convention uses the platform convention for at most eight integer
  or float arguments and one result. A C function saves the preserved
  registers it uses, and all of them when it calls an aihc function, since
  that call clobbers them.
- A narrow integer is canonical: an `iN` value is zero-extended to 64 bits.
  Signed operations sign-extend the operands first.
- `clz` and `ctz` are `clz` and `rbit` followed by `clz`. AArch64 has no
  population count of a general register, so `popcount` goes through the
  vector unit with `cnt` and `addv`, which the base architecture requires.
- A trap writes its message and a newline to the standard error stream and
  exits with status one.

### AMD64

`Aihc.Amd64.Lir` assembles the module with the direct ELF writer. It has the
design of the AArch64 backend:

- The allocator hands out `rax`, `rcx`, `rdx`, `rsi`, `rdi`, `r8`, and `r9`
  as volatile registers and `rbx` and `r12` to `r15` as preserved ones;
  `r10` and `r11` are the scratch registers of instruction selection. A
  function that divides or multiplies wide keeps `rax` and `rdx` for
  `rdx:rax` as well, and a function that shifts by a variable count keeps
  `rcx` for `cl`; every other function hands those to the allocator too. A
  two-operand instruction computes in place when its result already sits in
  the register of its left operand, which the allocator arranges when the
  operand dies there, and an addition of an immediate into another register
  is one `lea`.
- The `aihc` convention passes the first six arguments in `rdi`, `rsi`,
  `rdx`, `rcx`, `r8`, and `r9` and the rest in a 16-byte aligned block above
  the return address. The callee pops that block with `ret imm16`. Results
  come back in `rax`, `rdx`, `rcx`, `rsi`, `rdi`, `r8`, `r9`, and `r10`. An
  aihc function that calls nothing and spills nothing has no frame: the
  stack pointer stays on the return address. A tail call writes its
  outgoing block in place above the frame and moves the return address up
  when the block is no larger than the incoming one, moves the return
  address down to make room when it is larger and there is no frame, and
  otherwise builds the return address and the block below the frame and
  copies them up once the frame is gone.
- The `c` convention is the System V convention with at most six integer and
  eight float arguments and one result. A float travels as its bit pattern
  and moves through `xmm0` at the boundary. A C function saves the preserved
  registers it uses, and all of them when it calls an aihc function.
- `clz`, `ctz`, and `popcount` are `lzcnt`, `tzcnt`, and `popcnt`. This
  backend targets modern hardware and assumes SSE4.2, LZCNT, and BMI1; a host
  without them uses the LLVM backend.
- A trap writes its message and a newline to the standard error stream and
  exits with status one.

### LLVM

`Aihc.Llvm.Lir` renders textual LLVM IR that Clang compiles for the host:

- The `aihc` convention is `tailcc`, and a `tailcall` is a `musttail` call
  followed by `ret`, so LLVM verifies that the stack does not grow.
- Block parameters are `phi` instructions. Every edge with arguments goes
  through its own block, so a target reached twice from one predecessor has
  one `phi` entry per edge.
- The operations that trap check their operands and branch to a block that
  writes the message to the standard error stream and exits with status
  one.

### WebAssembly

`Aihc.Wasm.Lir` renders WebAssembly in the assembly syntax of LLVM MC, which
Clang assembles:

- Every value is a local. The narrow integer types, `ptr`, and `code` are
  `i32`; `i64`, `f32`, and `f64` are themselves. A `code` value is an index
  into the function table.
- A function is one loop with one nested block per Lir block and a
  `br_table` on the current block index. A jump assigns the parameters of the
  target and continues the loop.
- Both conventions are the WebAssembly convention. A `tailcall` is
  `return_call`, and `call.indirect` states the signature.
- `stack.alloc` reserves memory on the shadow stack below `__stack_pointer`.
- The full 64-bit multiplications are Lir helper functions that the backend
  adds to the module.
- A trap calls `aihc_lir_trap` with the message and its length and then
  executes `unreachable`. The WASI P3 host has no synchronous error stream,
  so its `aihc_lir_trap` drops the message and traps.

## Binary format

The binary format is not specified yet. It will use deterministic CBOR with a
symbol table and dense value indices.

## Example

```text
func @sum(%xs: ptr) -> i64 {
entry:
  jump loop(%xs, 0)

loop(%p: ptr, %acc: i64):
  %tag = load i8 [%p] align 1
  switch i8 %tag {
    0 -> done(%acc)
    1 -> cons(%p, %acc)
  }

cons(%cell: ptr, %sum: i64):
  %x = load i64 [%cell + 8] align 8
  %next = load ptr [%cell + 16] align 8
  %sum2 = add i64 %sum, %x
  jump loop(%next, %sum2)

done(%result: i64):
  return %result
}
```
