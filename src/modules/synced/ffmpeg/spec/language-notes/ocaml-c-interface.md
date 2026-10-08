# Language notes: the OCaml and C interface

Advice about writing these bindings in OCaml and C. It is not part of the
contract and it describes no design to copy: the normative files say what must
hold, and these notes say what the language makes easy to get wrong.

Statements about the runtime were checked against OCaml 5.5.0 only.

## Values held in C

- A C local of type `value` is invisible to the collector unless registered.
  Register every parameter and every intermediate value of a function that
  allocates, calls back into OCaml, or releases the runtime lock.
- A helper that returns a fresh value is safe by itself and unsafe as the
  argument of the next allocating call: store its result in a registered local
  first.
- `Store_field(block, i, allocating_call())` is sound: the runtime's macro
  evaluates the value into a temporary before it computes the field address.
- A helper that allocates on behalf of its caller takes a `value *` pointing
  at one of the caller's registered locals and writes through it, so that both
  see the value after it moves.
- A parameter that is an immediate today becomes a block when its type
  changes. Register it anyway.
- A closure about to be called after the runtime lock was released and retaken
  must be read again from its root.
- A function that only reads its arguments into C locals and then allocates
  nothing on the OCaml heap needs no registration, and can then be called
  where the lock state matters. Say so in its comment.

[binding-contract.md](../binding-contract.md) §2 rule B1 is the property;
running under a runtime that collects and compacts at every allocation is the
check.

## What may be stored in a block

- An integer stored in an OCaml block must be tagged. A structure of small C
  integers returned as a block of raw words is followed as pointers by the
  major collector.
- A pointer goes in a custom block or an abstract block, never in a field of
  an ordinary block.
- A null word is not an OCaml value. Do not pass the C integer `0` where a
  `value` is expected to mean "absent": give the native-pointer helper its own
  C function and let the two entry points call it.
- An optional argument reaches a stub as an option. Test it against the
  runtime's `None` constant and read its payload with the runtime's accessor.
  Applying a boolean or integer accessor to the option itself reads true for
  both `Some false` and `Some true`.
- A constant polymorphic variant is an immediate that is negative for about
  half of all names. Hold it in a signed, word-sized C type. A C enumeration
  type is typically unsigned and 32 bits wide and corrupts it.
- A polymorphic variant with an argument is a two-field block: the tag
  constant, then the payload.
- A constructor's argument that is an option needs the `Some` block around the
  payload. A tuple stored directly where an option belongs is read as
  `Some (its first field)`.
- A value handed to FFmpeg must be the native object, not the handle that
  wraps it; a value already converted to a variant must not be converted
  again. Both mistakes compile.

## Handles and finalisers

- A custom block holding one pointer is the usual handle. Its allocation call
  takes the size of the native memory it stands for; passing it is what makes
  the collector pace itself on frames and packets.
- The runtime has two kinds of finaliser with different rights.
  - The one attached to a custom block runs when the block's memory is
    reclaimed. It must not allocate, call back into OCaml, release the runtime
    lock or change roots.
  - A function registered on a value from OCaml may do all of those. It runs
    earlier, and may run while the value is still reachable from another value
    being finalised.
- A release that blocks, logs or drops roots needs the second kind's rights
  and the first kind's timing. The pattern that works is two stages: the
  OCaml-level function does the release proper and is idempotent; the custom
  block's finaliser only frees memory.
- Wrapping a native object in its handle before the fallible steps hands
  half-built objects to the finaliser. It must then tolerate every
  intermediate state: null members, a count set before the array it counts.
  Set counts as elements are filled.
- Wrapping last leaves the native object owned by nothing in between. Raising
  an OCaml exception from C skips every cleanup written after the raise: free
  first, raise last.
- Custom blocks compared or hashed with the default operations act on the
  holder. Two handles on the same static FFmpeg object are not structurally
  equal unless the handle type defines its own comparison.

## Roots held by native objects

- A closure stored in a native record needs a global root, registered when
  stored and removed when dropped, in the right order relative to freeing the
  record. Every failure branch between the two must remove it. Open paths have
  many such branches.
- A global root pins what the closure captures. A closure that captures the
  handle it is installed on then keeps that handle alive for ever.
- Both problems disappear for callbacks that FFmpeg only invokes during an
  operation on the handle: keep the closures in the OCaml value, reach them
  from C through the handle that the operation in progress has registered, and
  give FFmpeg an opaque pointer that leads to that registered slot. No global
  root is needed.
- A callback that FFmpeg may invoke from a thread of its own, outside any
  operation, does need a root with the lifetime of the installation.

## The runtime lock

- Release and acquire are separate statements with branches between them.
  Every exit path must leave the lock in the state its caller expects; taking
  it twice or releasing it twice does not fail at once on every platform.
- Before the release, copy out every input that lives on the OCaml heap:
  strings, record fields, the pointer inside a custom block. An accessor macro
  written in the argument list of the FFmpeg call is evaluated after the
  release.
- Retake the lock before raising, and before any conversion that can raise.
- A callback entered from FFmpeg takes the lock, calls the closure through the
  exception-catching entry point, converts the result, and releases the lock.
  An exception must not unwind through FFmpeg's frames.
- The result of the exception-catching entry point, when the closure raised,
  is an encoded exception and not a value. Left in a registered local across
  an allocation, it crashes the collector: replace it with the extracted
  exception before anything else.
- A callback cannot know by itself whether its thread holds the lock. The
  binding must: every FFmpeg call that can reach a callback is made with the
  lock released.
- A thread created by FFmpeg must be registered with the runtime before it
  takes the lock, and unregistered when the thread exits, not when the
  callback returns. A thread-specific key with a destructor does that, on one
  condition: the key is created before the runtime creates the key that
  holds its thread descriptor. glibc, musl and winpthreads clear the values
  of an exiting thread in key order before they call each destructor, and
  the runtime's unregistration returns at once when its own value is gone.
  The thread is then never detached; nothing fails, a leak detector shows
  it. A function run when the stubs are loaded creates the key early enough.
- With several domains the runtime lock is per domain. It serialises nothing
  between domains: use atomics for state that two domains can reach.

## Raising from C

- Exceptions defined in OCaml are reached from C through values registered by
  name at module initialisation.
- A message formatted into a static buffer is shared by every thread and every
  domain. Format into storage owned by the call.
- A stub declared as not allocating must not allocate, raise, or register
  local roots.

## Data kinds

- A bigarray allocated with a null data pointer is owned by the runtime. Its
  visible dimension can be reduced after the fact when fewer elements were
  written than were allocated.
- A bigarray over memory the runtime does not own has no finaliser of its own.
  To tie it to a reference-counted FFmpeg buffer, take a buffer reference and
  release it from an OCaml-level finaliser registered on the bigarray.
- Float arrays are read and written with the runtime's flat-float accessors,
  never copied as raw doubles. An empty float array built in C is not
  necessarily equal to `[||]` under structural comparison: return the atom
  OCaml uses.
- Reading the tag and the custom-operations identifier of a value is how a
  stub checks that a value has the representation its declared kind claims.
- A phantom type parameter costs nothing and checks nothing at run time.
  Assigning one with an unchecked cast is where a wrong value enters a typed
  program; an abstract witness or a GADT index makes the type system carry the
  check.

## Interface details

- A primitive with more than five arguments needs two entry points, one for
  bytecode and one for native code.
- An alert on an interface item must be attached to the item. A floating
  attribute written after it has no effect.
