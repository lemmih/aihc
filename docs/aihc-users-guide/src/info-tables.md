# Info tables

AIHC uses an info table to describe each type of object in managed memory.
An object header ordinarily points to one info table.
Thus, an object does not contain a copy of its run-time description.

Generated info tables are immutable.
The collector temporarily uses the header as a forwarding address when it moves an object.
The run-time system also makes temporary tables for blackholes.

The AMD64, ARM64, LLVM, and WebAssembly backends use the same info table model.
They use the common `AihcInfo` layout from the native run-time system.

## Table fields

Each info table contains these fields:

| Field | Purpose |
| --- | --- |
| `identity` | Identifies the function or the saturated constructor. |
| `entry` | Gives the portable run-time entry address. |
| `field_count` | Gives the number of payload fields in the object. |
| `remaining_arity` | Gives the number of arguments that the object still requires. |
| `field_is_pointer` | Points to a byte map that identifies managed pointers. |
| `next` | Points to the table for the next application stage. |
| `backend_entry` | Gives the entry address that has the backend calling convention. |
| `frame_kind` | Identifies a continuation frame for stack unwind operations. |
| `object_kind` | Identifies a node, closure, thunk, partial constructor, or special run-time object. |

The garbage collector uses `field_count` and `field_is_pointer` to find managed pointers.
The application code uses `remaining_arity` and `next` to apply one source argument.
The evaluation code uses `object_kind` and an entry field to enter an object.
Exception code uses `frame_kind` to identify continuation frames.

One source argument can use more than one machine field.
For this reason, two adjacent tables can have field counts that differ by more than one.
The `next` table gives the correct field layout for the next source argument.

## Application stages

A function has one info table for each required application stage.
An underapplication copies the object and its fields into a new object.
The new object header points to the `next` table.
A saturated closure can enter its generated code without this copy.

The table sequence records these changes:

```text
remaining arity:  2  ->  1  ->  0
stored fields:    0  ->  1  ->  2
```

The last table has no `next` value.
A saturated function can enter its generated code.

A constructor works differently.
It has two tables however many arguments it takes: one for the saturated node,
and one shared by every stage that still wants arguments.
An unsaturated constructor object stores in its first field the count of the
fields it holds, and its own fields follow from the second.
The shared table gives the saturated table as its `next` value, which is where
the run-time system reads the full width and the complete pointer map.
The slots an unsaturated constructor has filled are a prefix of the slots the
saturated one holds, so one pointer map serves both.
A saturated constructor is a data node, does not enter code, and stores no
count.

## Suspended function example

Consider a suspended call that captures `x` and `y`:

```haskell
makeTotal x y =
  let total = add x y
   in total
```

AIHC can represent this suspended call as a GRIN thunk for the generated `total` function.
The object stores the captured values in its payload.
Its table has the following logical values:

| Field | Value |
| --- | --- |
| `identity` | The generated `total` function. |
| `entry` | The portable entry for `total`. |
| `field_count` | `2` |
| `remaining_arity` | `0` |
| `field_is_pointer` | One byte for `x` and one byte for `y`. |
| `next` | Null. |
| `backend_entry` | The backend entry adapter for `total`. |
| `frame_kind` | No frame. |
| `object_kind` | Thunk. |

The pointer map depends on the run-time representation of `x` and `y`.
For example, a lifted value has a pointer byte of `1`.
An `Int#` value has a pointer byte of `0`.

When code evaluates `total`, it reads the thunk kind from the table.
It then uses the backend entry to call the generated function.
The run-time system sets the thunk state to blackhole during evaluation.
After evaluation, the run-time system changes the thunk into an indirection to the result.

## Constructor example

Consider this constructor:

```haskell
data Pair a b = Pair a b
```

`Pair` has arity two.
AIHC can represent `Pair`, `Pair first`, and `Pair first second` as managed objects.
The backend supplies two info tables:

| Table | Object kind | `field_count` | `next` |
| --- | --- | ---: | --- |
| `Pair` partial | Partial constructor | 2 | The saturated table. |
| `Pair` saturated | Node | 2 | Null. |

`Pair` and `Pair first` both use the partial table and differ only in the count
they store:

| Object | Stored count | Stored fields |
| --- | ---: | ---: |
| `Pair` | 0 | 0 |
| `Pair first` | 1 | 1 |
| `Pair first second` | None | 2 |

Both tables use the saturated constructor table as their `identity` value.
This stable identity lets case code compare a value with the `Pair` constructor.
Both tables also give the complete pointer map for both constructor fields.

Constructor application uses the common slow application path.
This path allocates the object for the next stage and copies the stored fields.
An application that fills the last field allocates a saturated node instead,
which drops the stored count.
The final object is a data node, so its entry addresses are null.

Nullary constructors have only the saturated table.
The backend also makes an implicit static object for each nullary constructor.

## Backend differences

The backends do not change the field meanings or the application sequence.
They change the table encoding and the backend entry implementation.

### AMD64

The AMD64 backend emits each table in read-only assembly data.
It emits a small entry adapter for each directly enterable object.
The `backend_entry` field points to this adapter.
Generated control code reads this field and jumps to the adapter.

### ARM64

The ARM64 backend uses the same table contents as the AMD64 backend.
It emits the tables and entry adapters with ARM64 sections, symbols, and calling conventions.

### LLVM

The LLVM backend emits constant values of the `%AihcInfo` type.
It puts a `tailcc` adapter in `backend_entry` for an enterable object.
It emits only the constructor tables that the current compilation unit requires.
Linked constructor table names keep constructor identities equal across compilation units.

### WebAssembly

The WebAssembly backend emits the same fields in WebAssembly assembly data.
Its pointers are 32 bits, but its counts and kind values stay 64 bits.
Thus, its table is 56 bytes instead of the 72-byte table on 64-bit native targets.

WebAssembly calls require a compatible function type.
Its run-time adapter converts `backend_entry` to the required function type.
The adapter then transfers control through the WebAssembly trampoline.

The portable `entry` field remains available for common run-time operations.
The backend entry gives the fast path for generated code.
