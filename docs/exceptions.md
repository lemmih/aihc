# Exception handling

## Goals

AIHC exceptions must use the same heap-resident continuation chain as every
other control transfer. They must not introduce a native stack, a second
exception stack, sentinel return values, or backend-specific Haskell semantics.

The intended source interface is the `base` interface built around `Exception`,
`SomeException`, `throw`, `throwIO`, `catch`, `try`, `mask`, and `throwTo`.
Synchronous exceptions must work in pure evaluation and `IO`. Asynchronous
exceptions must be delivered only where the compiler and runtime have made the
current computation interruptible.

## Current state

`raise#` and `catch#` first lower to direct `GrinThrow` and `GrinCatch` nodes.
The FC and direct-GRIN interpreters retain their out-of-band reference
behavior. CPS-GRIN now eliminates both direct nodes: catches allocate an
explicit catch frame and raises become `GrinCpsRaise` transfers. The shared
runtime walks the continuation chain and every backend tail-resumes the
selected handler through its existing transfer convention.

Structural `Typeable` evidence and checked casts are implemented. The typed
library layer now provides `Exception`, `SomeException`, `throw`, `throwIO`,
selective `catch`, `handle`, `try`, their `Just`-selecting variants, and
`evaluate`. Nonmatching typed catches rethrow the original `SomeException`.
Class dictionaries retain superclass fields across separately compiled modules,
so the existential `Exception` dictionary supplies the `Typeable` evidence used
by `fromException`. Class default methods provide the standard empty user
instance declaration. AIHC does not have `Show` yet, so its initial `Exception`
class has the `Typeable` superclass and defaults `displayException` to the type
constructor name; adding `Show` can align that remaining superclass and default
with GHC without changing the exception representation.

The CPS pass now reifies the rest of a computation as an ordinary closure whose field zero
is always its parent continuation. CPS records a frame kind for every generated
continuation, GC-GRIN preserves it, and every backend emits it in `AihcInfo`.

Primitive and typed synchronous catch, nested rethrow, and update-frame
unwinding work. Masking, asynchronous delivery, and the masking-dependent
cleanup combinators remain.

## Continuation-chain invariant

Every unapplied continuation closure must have its parent continuation in field
zero. The CPS pass must capture the parent even when a later optimization could
otherwise prove that field dead. The generated continuation function receives
the same field first. Update continuations change from `[blackhole, parent]` to
`[parent, blackhole]`.

Continuation info tables carry a small, backend-independent frame kind:

```text
normal          [parent, captures...]
catch           [parent, handler]
update          [parent, blackhole]
restore-mask    [parent, previous-mask-state]
stop            [no parent]
```

Only functions identified by `CpsGrinProgram` as continuations receive this
metadata. An ordinary closure is never treated as a frame merely because its
first field happens to be a pointer. The frame metadata must be preserved by
GC-GRIN and emitted into the shared `AihcInfo` ABI by every backend.

Appending a frame-kind field to `AihcInfo` is preferable to backend callbacks.
The unwinder then has a fixed set of portable operations, including on Wasm,
and arbitrary Haskell code is never run while the runtime is inspecting the
chain.

The invariant applies to unapplied frames. Applying a continuation may advance
its closure info table and append result fields, but no safe point is allowed in
that administrative transition before its generated entry tail-transfers.

## Synchronous exceptions

The CPS pass eliminates `GrinThrow` and `GrinCatch` rather than rejecting them.

For `catch# action handler`, with outer continuation `k`, it allocates a catch
frame containing `k` and `handler`, then applies `action` with that frame as its
continuation. Normal application of the frame forwards the action's result to
`k`.

For `raise# exception`, with current continuation `k`, it emits a CPS raise
transfer. The shared runtime walks `k`:

1. A normal frame is skipped by following field zero.
2. An update frame performs blackhole cleanup, then follows its parent.
3. A restore-mask frame restores the saved masking state, then follows its
   parent.
4. A catch frame is popped. The current mask state is saved in a restore-mask
   frame above its parent, the handler state becomes at least
   `MaskedInterruptible`, and the handler is applied to the exception with that
   restore frame as its continuation.
5. A stop frame reports an uncaught exception or terminates an unhandled child
   thread, depending on the stop kind.

Popping the catch frame before entering the handler is essential: an exception
raised by that handler must continue to the next outer handler. The restore
frame makes the implied mask around every exception handler hold on both normal
return and exceptional exit; an already `MaskedUninterruptible` handler remains
uninterruptible.

The runtime returns an ordinary `AihcResume`/portable transfer describing the
handler application. Native code tail-enters that transfer. It must not call a
handler and return through C or assembly frames.

The walk itself does not allocate or invoke GC. The machine roots the exception
and current frame for its duration. A caught frame can be retagged in place as
the restore-mask frame after its handler field has been loaded, and the current
blackhole object already reserves field zero for an indirection. Avoiding an
emergency allocator makes the unwinder valid even when the raise originated at
an allocation failure boundary.

### Update frames and blackholes

Unwinding an update frame cannot leave a blackhole owned by a computation that
no longer exists.

- A synchronous failure should replace the thunk with a small runtime raise
  thunk holding the exception, remove the blackhole record, and wake waiters.
  Re-entering the thunk then raises the same exception through the new current
  continuation.
- An asynchronously interrupted evaluation should restore the original thunk
  tag and remove the blackhole record. Waiters retry evaluation instead of
  permanently memoizing an interruption that was directed at another thread.

Waking a waiter therefore has two explicit resume forms. A memoized synchronous
failure resumes it with `Raise exception waiterContinuation`; an asynchronous
rollback resumes it with `Eval originalThunk waiterContinuation`. Neither path
pretends that the exception is the thunk's returned value.

The original thunk info pointer and environment survive the current blackhole
representation, so restoring the thunk requires no copy. The distinction is a
property of how the raise was initiated, not of the exception's Haskell type:
explicitly calling `throwIO UserInterrupt` is still a synchronous raise.

The current first implementation restores the original thunk and resumes
existing blackhole waiters by raising through each waiter's continuation. This
prevents a stranded blackhole and preserves retryability, but does not yet
memoize the synchronous exception for future entries. Installing the portable
raise thunk described above remains part of completing synchronous exception
semantics.

## `Typeable`, `Exception`, and `SomeException`

`SomeException` remains an ordinary Haskell existential node:

```haskell
data SomeException = forall e. Exception e => SomeException e
```

It requires no runtime tag. The contained `Exception e` dictionary retains the
operations and `Typeable e` evidence needed for display and typed projection.
The primitive catch frame catches every `SomeException`; the source-level typed
`catch` uses `fromException` and rethrows a nonmatching value.

`Typeable` must not base a sound cast on a user-assigned integer or an unchecked
hash. The initial implementation uses a structural representation:

```text
TypeRep(TyConIdentity, [argument TypeRep...])
```

The type checker synthesizes evidence from `TcType`. Evidence for `T a` is
composed from evidence for `a`, so polymorphic code can build representations
from given dictionaries. `cast` compares complete trees and invokes the one
trusted lifted-value `unsafeCoerce#` only after equality succeeds.

A `TyCon` carries the package, the module and the name of the constructor,
laid out as GHC's `GHC.Types.TyCon`, and two of them are equal when all three
agree. GHC compares fingerprints instead; aihc builds none, and the qualified
name identifies a constructor just as well. The kind arity and the kind
representation of a `TyCon` are placeholders: `Typeable` evidence does not
carry the kind of the constructor, and nothing in aihc reads either field
back.

The compatible library layer provides:

- automatic `Typeable` evidence, including functions, lists, tuples, and
  parameterized user types;
- superclass dictionary projection and class default methods where required by
  the standard `Exception` definition;
- `Exception`, `SomeException`, `toException`, `fromException`, and
  `displayException`;
- `throw`, `throwIO`, typed `catch`, `handle`, `try`, and `evaluate`.

`finally`, `bracket`, and the masking API remain coupled to the masking-state
step: exposing cleanup combinators before masking would give them the wrong
asynchronous-exception semantics.

The initial AIHC `Typeable` dictionary contains compiler-supplied `typeRep` and
`typeOf` fields. Once imported class-selector metadata is retained reliably,
`typeOf` should become the ordinary library wrapper used by `base`, leaving a
single representation field in the dictionary.

Default `toException` constructs `SomeException`; default `fromException`
structurally casts the contained value. User exception values and
`SomeException` are always ordinary GC-traced heap objects.

## Asynchronous exceptions

### Per-thread state

A pending asynchronous exception belongs to a green thread, not to the whole
machine. Each `AihcThread` needs:

- a FIFO queue of pending exception values;
- `Unmasked`, `MaskedInterruptible`, or `MaskedUninterruptible` state;
- enough blocked-operation bookkeeping to remove it from an IO, MVar, or
  blackhole wait queue;
- optional sender acknowledgements for `throwTo`.

Pending exception values and any blocked sender continuations are collector
roots. Deliver at most one queued exception at a safe point; another may be
delivered at a later safe point after the handler restores the appropriate
masking state.

`throwTo target exception` queues the exception for `target`. A fully compatible
implementation blocks the sender until the target accepts delivery. Delivery
to the current thread can raise immediately. Killing a blocked thread first
detaches or cancels its pending operation, then resumes it with a raise transfer.

### Safe points

A safe point is an explicit CPS operation with access to the current
continuation. Checking a flag after arbitrary generated instructions is not
safe, because locals may not describe a resumable Haskell state.

Polls carry a reason such as `Allocation`, `Yield`, `InterruptibleWait`, or
`AllowInterrupt`. Delivery is a small table: `Unmasked` accepts at every poll,
`MaskedInterruptible` accepts only at an interruptible wait or explicit
`allowInterrupt`, and `MaskedUninterruptible` never accepts. This keeps masking
policy out of backend-specific branches.

Initial safe points should be:

- every allocation check, whether or not that check actually invokes GC;
- `yield#`;
- entry to an interruptible wait in `awaitIO#`, MVar operations, or blackhole
  waiting;
- scheduler resumption before a thread re-enters generated code.

This suggests an explicit CPS/GC-GRIN poll carrying `k`, rather than hiding
delivery inside `GrinEnsureHeap`. The GC root set already keeps live values and
`k` precise; the poll makes the control effect visible to lint and every
backend. A poll where delivery is disallowed returns normally. A deliverable
pending value abandons the current generated entry and returns the same raise
transfer used by synchronous exceptions, tagged with asynchronous origin for
update-frame cleanup.

`MaskedInterruptible` permits delivery only at interruptible waits;
`MaskedUninterruptible` never permits it. Mask changes need restore-mask frames
so both normal return and exception unwinding restore the previous state.
Library combinators such as `bracket` are then expressed with `mask`, `catch`,
and rethrowing rather than runtime-specific cleanup callbacks.

Changing the mask state and installing its restore frame must be one runtime
operation. There can be no delivery window between the two. Likewise, entering
an interruptible wait atomically records the blocked operation before polling,
so delivery can always detach exactly one registered wait.

### Ctrl-C and host events

A POSIX signal handler must not allocate, traverse the heap, take a lock, or
enter generated code. It only records a `sig_atomic_t` event and wakes a blocked
poller using an async-signal-safe mechanism. The scheduler or the next
allocation poll converts that event into a pending `UserInterrupt` for the main
green thread. A preallocated exception value may be rooted by the machine so
this conversion does not depend on allocation at signal time.

The same runtime entry point should accept a user-interrupt event from non-POSIX
hosts. Wasm exception semantics remain identical even when a particular WASI
runner cannot translate terminal Ctrl-C into a component event. Deterministic
cross-backend tests use `throwTo`; signal-adapter tests are separate host
integration tests.

If a thread is blocked in the IO manager, a signal wakes the poll loop, removes
or cancels its request, and enqueues a raise resume. Request ownership and
buffer lifetime must still reach a terminal state; abandoning a continuation
must not leak a submitted request indefinitely.

## Uncaught exceptions

The main thread's stop frame routes an uncaught `SomeException` to a Haskell top
handler so `displayException` and exit status are portable. A child thread's
stop frame terminates that child and releases its runtime records; later work
may add the same reporting policy as GHC. Runtime corruption and impossible
frame layouts remain hard runtime failures, not catchable Haskell exceptions.

## Validation

The feature needs proof at several layers:

- CPS unit tests that every generated continuation has one parent link and that
  catch, update, mask, and stop frames have the declared layouts;
- CPS boundary tests showing that source throw/catch nodes are eliminated;
- runtime tests for nested handlers, handler rethrow, update-frame cleanup,
  masking restoration, blocked-thread removal, FIFO delivery, and GC while
  exceptions are pending;
- shared FC/GRIN fixtures for `Typeable`, `SomeException`, selective typed
  catches, pure exceptions forced by `evaluate`, and lazy handlers;
- backend code-generation tests for frame metadata and raise/poll transfers;
- portable examples compiled by every backend.

The `exceptions-sync` portable example uses two user exception types and nested
typed catches. The inner handler does not match, so `catch` rethrows the
original `SomeException` for the outer handler.

The remaining asynchronous example should:

- fork a worker, demonstrate masked cleanup and restore, send an exception with
  `throwTo`, and deterministically join through an `MVar`.

A separate POSIX test launches a looping executable, sends it `SIGINT`, and
checks `UserInterrupt` handling. It is not the semantic cross-backend test.

## Implementation sequence

1. **Complete:** land structural `Typeable`, exact comparison, and the trusted
   `unsafeCoerce#` primitive as an independent change.
2. **Complete:** make the continuation parent link and frame metadata explicit
   without changing behavior.
3. **In progress:** primitive and typed synchronous raise/catch lowering,
   `Exception`, `SomeException`, shared unwinding, and rollback-style update
   cleanup are implemented. Portable exception memoization remains.
4. Add masking state and restore frames, then implement the library masking and
   cleanup combinators.
5. Add per-thread pending queues, safe-point polls, blocked-operation
   cancellation, and deterministic `throwTo` tests.
6. Add POSIX Ctrl-C and other host event adapters.

Each step preserves the single-control-stack invariant and can be reviewed
without requiring every async source to land at once.
