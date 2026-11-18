# Binding contract

The rules every synced C binding follows, whatever library it binds. It owns
handle lifetime, the boundary between the OCaml runtime and C, enumeration
tables, argument and result checking, the shared error rules, the runtime
lock, the use guard, threads the C library creates, callbacks and data
transfer. It relies on [concurrency.md](../../../spec/concurrency.md) §4 for
resources, and on the vocabulary of the [README](README.md#vocabulary).

A binding's own contract answers each section under the same number: the
handles it has, the operations that release the lock, the handles that are
guarded, its callbacks, its paths of data transfer. A section it does not
need is answered "nothing".

## 1. Scope

The rules hold for every OCaml-visible value that stands for a native object
and every stub that crosses the boundary. A binding states, for each of its
libraries, the C libraries it binds, what it uses from other binding
libraries and what it provides to them.

## 2. Objects

### 2.1 Kinds of handle

Every OCaml-visible type that stands for a native object is an owned, a
borrowed or a dependent handle. Each binding lists its handles with: the
native object, how the handle is created, what it owns, what it keeps alive,
how it is released, and its states.

### 2.2 Lifetime rules

- **L1. One owner, one release.** Every native object the binding allocates
  has exactly one owner at any time, and is released exactly once, by the
  release call the C library defines for its kind.
- **L2. No unowned object across a failure point.** When a constructor fails
  at any step, everything it allocated is released before the failure reaches
  the caller, and no handle to a partly built object exists. This holds for
  failures of C library calls, of allocations, and for exceptions raised by
  user functions the constructor calls.
- **L3. A dependent handle keeps its owner alive.** While a dependent handle
  is reachable, its owner is not released by collection. An operation on a
  dependent handle either succeeds or raises a state error (§5.3); it never
  reaches a released object.
- **L4. Nothing outlives what it points into.** A value that shares memory
  with a native buffer (§8) keeps that buffer alive for as long as the value
  is reachable.
- **L5. Explicit close is idempotent.** Closing a closed handle does nothing.
  After a close, the handle is a valid OCaml value in the closed state.
- **L6. Close always releases.** An explicit close releases the native object
  even when a step of the close fails. It then raises the first failure. No
  failure leaves a handle that can be neither used nor closed.
- **L7. Collection releases without ceremony.** Release by collection frees
  the native resources. It performs no protocol step (no flush, no trailer,
  no final write), it raises nothing, and it MUST be safe whatever the
  object's state, including half-way through a failed operation.
- **L8. Release order.** A user closure installed on an object may be invoked
  during the object's release and MUST NOT be invoked after it. What keeps the
  closure alive is dropped after the last native call that can invoke it.
- **L9. Native memory is accounted for.** A handle that holds significant
  native memory SHOULD report its size to the garbage collector. A binding
  names the handles for which this is a MUST.
- **L10. One allocator family.** Memory that crosses the boundary between the
  binding and the C library is allocated with the allocator the library
  expects. When the library may replace a buffer it was given, the pointer
  released is the current one.

### 2.3 Boundary rules

These constrain the stubs. They are stated as properties a test can check;
[language-notes/ocaml-c-interface.md](language-notes/ocaml-c-interface.md)
says how the language is usually made to satisfy them.

- **B1. Any collection, anywhere.** Every operation MUST be correct when the
  garbage collector runs, and moves every movable value, at each allocation
  the operation performs, at each call back into OCaml, and at each point
  where the runtime lock is released.
- **B2. Values round-trip.** An integer, enumeration value or flag read
  through the binding equals the value the C library holds. An enumeration
  value is converted exactly once on each crossing.
- **B3. Types are trusted only where they cannot lie.** Where the interface
  lets a caller choose a phantom type parameter freely, or build a value of a
  public record or module type by hand, every operation MUST check at run
  time whatever its memory safety depends on, and raise on a mismatch. Each
  binding lists these places.

## 3. Enumeration tables

C enumerations and flag sets are OCaml polymorphic variants, through tables
generated from the headers ([build.md §3](build.md#3-generated-code)) or
written by hand. A binding lists its hand-written tables in full.

- **E1.** A hand-written table refers to each C constant by its name in the
  headers. A literal number standing for a C constant is not allowed.
- **E2.** OCaml to C is total: every constructor of the type has an entry.
- **E3.** C to OCaml, for a single value with no entry (a value from a C
  library newer than the headers the binding was built with): the operation
  raises the binding's failure error, and its message names the table and the
  value. Lookups the binding lists (F4) raise `Not_found` instead.
- **E4.** C to OCaml, when building a collection (the flags of a bit mask, a
  list of supported values, an enumeration of registered components): an
  element with no entry is left out and the operation succeeds.
- **E5.** When several constructors share one C value, C to OCaml yields the
  one declared first in the header.
- **E6.** A bit mask converts to the list of its flags in ascending order of
  bit value. A flag list converts to the bitwise OR of its members; the empty
  list is 0.
- **E7.** A library's "none" and "unknown" members are ordinary
  constructors. Where an operation's result type is an option, the binding
  states which C value is `None`.

## 4. Operations

- **A1. Arguments are validated.** Every index, offset, length, count, array
  length and buffer shape supplied by the caller is checked before any native
  memory is read or written through it. A negative, out-of-range or
  inconsistent value raises the binding's failure error.
- **A2. Collections come in the library's order.** A list built from a C
  iteration, array or dictionary is in the library's order unless the
  operation states another.
- **A3. Absent text.** A text field the library leaves null is `None` where
  the result is a `string option` and the empty string where it is a
  `string`.
- **A5. Results are fresh.** A value returned to the caller shares no mutable
  state with a value returned by another call, except where §8 states that
  memory is shared.
- **A6. Text ends at a NUL.** An OCaml string passed where the library expects
  text ends at its first NUL byte.
- **A7. Every result code is examined.** No C return value that can signal
  failure is discarded.

## 5. Errors

### 5.1 Exceptions

Each binding defines its exceptions and the mapping from the library's error
codes, and states which value is its **failure error**, the one raised for a
condition the binding detects itself.

- **F3.** A failed allocation by the binding raises `Out_of_memory`. An error
  code the C library reports keeps its mapping, even when the library names
  it after memory: libraries use such codes for more than allocation.
- **F4.** `Not_found` is raised only by the lookups each binding lists. No
  operation raises `Stdlib.Failure`, `Invalid_argument` or `Assert_failure`.
- **F5.** An exception raised by a user function called synchronously by an
  operation (§7.2) propagates unchanged.

### 5.2 State after a failure

- **F6.** After an operation raises, every handle is in a state its binding
  defines: unchanged, closed, or failed. Nothing the failed operation
  allocated remains (L2).

### 5.3 State errors

A binding whose handles can be closed, failed or in use raises a distinct
error for each: the handle or its owner is closed; the handle or its owner
is failed; the handle is in use by another operation (§6.2). The binding
states the values raised and whether their texts are binding.

## 6. Blocking and concurrency

### 6.1 The runtime lock

- **M1.** An operation MUST NOT hold the runtime lock while it waits for input
  or output, or while the C library does work that can take long (opening,
  probing, coding, converting, device set-up). Each binding lists the
  operations this covers. Other OCaml threads run meanwhile.
- **M2.** While the lock is released, the operation reads and writes no memory
  managed by the OCaml heap. Its inputs are immediates, copies made
  beforehand, or native objects.
- **M3.** The lock is in the state the caller left it on every exit path,
  failures included. A failure inside the unlocked section reaches the caller
  as an exception, and other threads were able to run throughout.
- **M4.** A C call that can reach a callback of §7.1 is made with the lock
  released.

Which of the remaining operations release the lock is not specified.

### 6.2 Use of one object from several threads

A program may run several threads and several domains.

- **M5. Stateful handles are memory-safe under any interleaving.** For a
  handle with a guard (M6), no interleaving of operations, from any number of
  threads or domains, may corrupt memory or crash.
- **M6. Use guard.** Every handle on a native object whose state the library
  does not protect against concurrent use has a guard. An operation that
  changes the object's structure or state takes it exclusively; an operation
  that only reads takes it shared. An operation that finds the guard taken in
  a conflicting way raises the in-use error of §5.3 at once. It does not
  wait.
- **M7.** The guard is held for one native operation, including the time the
  runtime lock is released and any callback of §7.1 that runs inside it. It
  is not held across an operation composed of other operations, nor while a
  synchronous user function of §7.2 runs.
- **M9.** A dependent handle shares its owner's guard.
- **M12. The guard must not be felt.** Taking and releasing a guard costs a
  constant, small number of uncontended atomic operations and never blocks.
  An implementation whose guards measurably slow its hot loop does not
  conform.

Each binding lists, per guarded handle, which operations are exclusive and
which are shared, and the values it leaves unguarded with the reason.

### 6.3 Threads the C library creates

- **M10.** A C function that the library may call on a thread unknown to the
  OCaml runtime registers that thread before touching OCaml state, and the
  registration is undone when the thread exits, not when the function
  returns.
- **M11.** The library's own threads enter OCaml only through the callbacks
  of §7.1.

### 6.4 Global state

Each binding lists what it initialises at load time and any process-wide
state it keeps. Process-wide state is safe to reach from several domains
([concurrency.md §3](../../../spec/concurrency.md#3-shared-state)).

## 7. Callbacks

### 7.1 Functions the C library calls

A user closure that the library invokes during one of its own calls is
called through this sequence:

1. Register the current thread (M10).
2. Acquire the runtime lock.
3. Call the closure, catching any exception.
4. Translate the result to the C return value the library's callback contract
   expects.
5. Release the runtime lock.

- **C1.** An exception never unwinds through C. It becomes the error return
  each binding states for that callback.
- **C2.** A result the closure returns outside the range its contract allows
  is treated as a failure of the closure.
- **C3.** The closure is kept alive from installation until the object is
  released or the closure is replaced, and no longer (L8).
- **C4.** A closure of this kind runs while the operation that triggered it
  holds the object's guard: an operation on that object called from the
  closure raises the in-use error (M7). A closure that a thread of the
  library's own invokes outside any operation finds the guard free or taken,
  as it happens.

Each binding states, per callback: what triggers it, on which thread it may
run, the value an exception turns into.

### 7.2 Functions the binding calls

A user function passed to an operation and called by the binding itself,
between native calls, runs on the caller's thread with the runtime lock held.

- **C5.** Its exceptions propagate to the caller of the operation (F5).
- **C6.** The object is not in use while it runs (M7): it may call any
  operation on the object.
- **C7.** Each binding states what state the object is in when the function
  is called and after it raises.

## 8. Data transfer

For each path that moves media data or text across the boundary, the binding
states whether the bytes are **copied** or **shared**.

- **S1.** OCaml strings and byte sequences are copied in both directions.
- **S2.** A bigarray MAY share memory with a native buffer. It then keeps the
  buffer alive (L4), and its length is exactly the number of elements the
  buffer validly holds.
- **S3.** A value built by an operation for its caller is not written again by
  the binding after the operation returns.
