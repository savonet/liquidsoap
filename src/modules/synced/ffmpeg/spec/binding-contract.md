# Binding contract

The rules all seven binding libraries share. Each library has its own file
with the same eleven sections in the same order, so "§6" is the same topic
everywhere. A library file answers every clause of this contract for its
library; a clause it does not answer is a gap in that file.

| Library      | C libraries bound                                     | File                           |
| ------------ | ----------------------------------------------------- | ------------------------------ |
| `avutil`     | libavutil (and libavcodec for the subtitle structure) | [avutil.md](avutil.md)         |
| `avcodec`    | libavcodec                                            | [avcodec.md](avcodec.md)       |
| `av`         | libavformat                                           | [avformat.md](avformat.md)     |
| `avfilter`   | libavfilter                                           | [avfilter.md](avfilter.md)     |
| `avdevice`   | libavdevice                                           | [avdevice.md](avdevice.md)     |
| `swresample` | libswresample                                         | [swresample.md](swresample.md) |
| `swscale`    | libswscale                                            | [swscale.md](swscale.md)       |

The key words MUST, MUST NOT, SHOULD, SHOULD NOT and MAY are used as in
RFC 2119. A value written `LIKE_THIS` is a named parameter; its recommended
value is given where it is defined.

## 1. Scope

### 1.1 Libraries

Dependencies between the binding libraries:

    avutil  <-  avcodec  <-  av  <-  avdevice
       ^           ^
       |           +---  swresample
       +---  avfilter
       +---  swscale

Each library is one OCaml top module. Its §1 states the C libraries it binds,
what it uses from its dependencies and what it provides to its dependents.

### 1.2 Frozen surface

These are fixed. A rule elsewhere that would change one of them is wrong.

1. **The OCaml interface.** Each library file's §4 gives the interface in
   full: every type, value and module, with its signature. That text is the
   interface. Nothing is added to it or removed from it.
2. **The FFmpeg range.** [compatibility.md](compatibility.md) §1.
3. **The OCaml range.** [compatibility.md](compatibility.md) §5.
4. **Cross-compilation.** The bindings build for Windows through
   `dune -x windows`, as standalone opam packages and embedded in a
   workspace, with the environment variables of
   [cross-compilation.md](cross-compilation.md) §3.2.
5. **Names.** The eight opam packages and library names
   ([build.md](build.md) §1), the installed C header file names
   ([build.md](build.md) §4) and the test alias `ffmpeg_citest`
   ([build.md](build.md) §5).

### 1.3 Module initialisation

Loading a library's module runs its initialisation: reading versions and
constants, enumerating what FFmpeg registers.

- **I1.** Initialisation MUST succeed against every FFmpeg build that passes
  detection, whatever it contains: components with absent text fields, option
  types and enumeration values newer than the bindings, external codecs and
  filters. It walks whatever the installed FFmpeg registers, so one unusual
  component would otherwise stop every program at start.
- **I2.** Initialisation MAY fail only when FFmpeg lacks something the
  library's interface names as a value. Each library's §1 lists those.

## 2. Objects

### 2.1 Kinds of handle

Every OCaml-visible type that stands for a native object is one of three
kinds.

**Owned handle.** The OCaml value owns the native object. The object is
released when the value is collected, or earlier by an explicit close where
the interface has one.

**Borrowed handle.** The OCaml value holds a pointer to static FFmpeg data (a
codec, a format, an option class, a pixel-format descriptor). It is valid for
the life of the process and is never released.

**Dependent handle.** A value that designates part of another object, its
**owner**: a stream of a container, a filter of a graph, a plane of a frame,
an option-bearing object inside a container.

Each library's §2 lists its handles with: the native object, how the handle
is created, what it owns, what it keeps alive, how it is released, and its
states.

### 2.2 Lifetime rules

- **L1. One owner, one release.** Every native object the binding allocates
  has exactly one owner at any time, and is released exactly once, by the
  release call FFmpeg defines for its kind.
- **L2. No unowned object across a failure point.** When a constructor fails
  at any step, everything it allocated is released before the failure reaches
  the caller, and no handle to a partly built object exists. This holds for
  failures of FFmpeg calls, of allocations, and for exceptions raised by user
  functions the constructor calls.
- **L3. A dependent handle keeps its owner alive.** While a dependent handle
  is reachable, its owner is not released by collection. An operation on a
  dependent handle either succeeds or raises a state error of §5.3; it never
  reaches a released object.
- **L4. Nothing outlives what it points into.** A value that shares memory
  with a native buffer (§8) keeps that buffer alive for as long as the value
  is reachable.
- **L5. Explicit close is idempotent.** Closing a closed handle does nothing.
  After a close, the handle is a valid OCaml value in the **closed** state.
- **L6. Close always releases.** An explicit close releases the native object
  even when a step of the close fails. It then raises the first failure. No
  failure leaves a handle that can be neither used nor closed.
- **L7. Collection releases without ceremony.** Release by collection frees
  the native resources. It flushes no encoder and writes no trailer, it raises nothing, and it MUST be safe
  whatever the object's state, including half-way through a failed operation.
- **L8. Release order.** A user closure installed on an object may be invoked
  during the object's release and MUST NOT be invoked after it. What keeps the
  closure alive is dropped after the last native call that can invoke it.
- **L9. Native memory is accounted for.** A handle on a frame or a packet MUST
  report the size of the native memory it holds to the garbage collector,
  whichever operation produced it. Handles on other objects that hold
  significant native memory SHOULD.
- **L10. One allocator family.** Memory that crosses the boundary between the
  binding and FFmpeg is allocated with FFmpeg's allocator. When FFmpeg may
  replace a buffer it was given, the pointer released is the current one.

### 2.3 Boundary rules

These constrain the stubs. They are stated as properties a test can check;
[language-notes/ocaml-c-interface.md](language-notes/ocaml-c-interface.md)
says how the language is usually made to satisfy them.

- **B1. Any collection, anywhere.** Every operation MUST be correct when the
  garbage collector runs, and moves every movable value, at each allocation
  the operation performs, at each call back into OCaml, and at each point
  where the runtime lock is released.
- **B2. Values round-trip.** An integer, enumeration value or flag read
  through the binding equals the value FFmpeg holds. An enumeration value is
  converted exactly once on each crossing.
- **B3. Types are trusted only where they cannot lie.** Phantom type
  parameters carry the media kind, the direction and the mode. Where the
  interface lets a caller choose a phantom parameter freely, or build a value
  of a public record or module type by hand, every operation MUST check at
  run time whatever its memory safety depends on, and raise on a mismatch.
  Each library's §2 lists these places.

## 3. Enumerations and constants

FFmpeg enumerations are OCaml polymorphic variants. Two sources:

- **Generated tables**, derived from the target's FFmpeg headers
  ([build.md](build.md) §3). The OCaml type and both conversion directions
  come from the same table.
- **Hand-written tables**, for small flag sets and for enumerations the
  generator does not cover. Each library lists them in full in its §3.

Rules for every table:

- **E1.** A hand-written table refers to each C constant by its name in the
  headers. A literal number standing for a C constant is not allowed.
- **E2.** OCaml to C is total: every constructor of the type has an entry.
- **E3.** C to OCaml, for a single value with no entry (a value from an FFmpeg
  library newer than the headers the bindings were built with): the operation raises ``Error (`Failure msg)`` and `msg` names the table and
  the value. The lookups of F4 raise `Not_found` instead.
- **E4.** C to OCaml, when building a collection (the flags of a bit mask, a
  list of supported values, an enumeration of registered components): an
  element with no entry is left out and the operation succeeds.
- **E5.** When several constructors share one C value, C to OCaml yields the
  one declared first in the header.
- **E6.** A bit mask converts to the list of its flags in ascending order of
  bit value. A flag list converts to the bitwise OR of its members; the empty
  list is 0.
- **E7.** FFmpeg's "none" and "unknown" members are ordinary constructors.
  Where an operation's result type is an option, the library states which C
  value is `None`.

## 4. Operations

Each library's §4 gives every item of its interface, in the interface's
order, with its signature and its behaviour. Rules for every operation:

- **A1. Arguments are validated.** Every index, offset, length, count, array
  length and plane shape supplied by the caller is checked before any native
  memory is read or written through it. A negative, out-of-range or
  inconsistent value raises ``Error (`Failure msg)``.
- **A2. Collections come in FFmpeg's order.** A list built from an FFmpeg
  iteration, array or dictionary is in FFmpeg's order unless the operation
  states another.
- **A3. Absent text.** A text field FFmpeg leaves null is `None` where the
  result is a `string option` and the empty string where it is a `string`.
- **A4. Inputs are not consumed.** An operation that hands a frame or a packet
  to FFmpeg leaves the caller's value unchanged and usable. The binding gives
  FFmpeg a reference of its own.
- **A5. Results are fresh.** A value returned to the caller shares no mutable
  state with a value returned by another call, except where §8 states that
  memory is shared.
- **A6. Text ends at a NUL.** An OCaml string passed where FFmpeg expects text
  ends at its first NUL byte.
- **A7. Every result code is examined.** No FFmpeg return value that can
  signal failure is discarded.

## 5. Errors

### 5.1 One exception

`Avutil.Error` carries every failure of the bindings. [avutil.md](avutil.md)
§5 owns its definition and the mapping from FFmpeg error codes.

- **F1.** A negative return from an FFmpeg call raises `Error` with the mapped
  code.
- **F2.** A condition the binding detects itself raises
  ``Error (`Failure msg)``. `msg` is for a human reader; its wording is not
  part of the contract except for the texts of §5.3.
- **F3.** A failed allocation raises `Out_of_memory`.
- **F4.** `Not_found` is raised only by the lookups each library lists in its
  §5. `Avfilter.Exists` is the one exception a library defines besides
  `Avutil.Error`. No operation raises `Stdlib.Failure`, `Invalid_argument` or
  `Assert_failure`.
- **F5.** An exception raised by a user function called synchronously by an
  operation (§7.2) propagates unchanged.

### 5.2 State after a failure

- **F6.** After an operation raises, every handle is in a state the
  library's §2 defines: unchanged, closed, or failed. Nothing the failed
  operation allocated remains (L2).
- **F7.** A failure of an operation that takes `?opts` leaves the caller's
  option table untouched.

### 5.3 State errors

| State                                            | Raised value                             |
| ------------------------------------------------ | ---------------------------------------- |
| the handle, or its owner, is closed              | ``Error (`Failure "Container closed!")`` |
| the handle, or its owner, is failed              | ``Error (`Failure "Object failed!")``    |
| the handle is in use by another operation (§6.2) | ``Error (`Failure "Object in use!")``    |

These three texts are part of the contract.

## 6. Blocking and concurrency

### 6.1 The runtime lock

- **M1.** An operation MUST NOT hold the OCaml runtime lock while it waits for
  input or output, opens or probes a container, opens a codec, sends to or
  receives from a codec or a filter, configures or runs a filter graph,
  converts media data, or initialises or transfers to a hardware context.
  Other OCaml threads run meanwhile.
- **M2.** While the lock is released, the operation reads and writes no memory
  managed by the OCaml heap. Its inputs are immediates, copies made
  beforehand, or native objects.
- **M3.** The lock is in the state the caller left it on every exit path,
  failures included. A failure inside the unlocked section reaches the caller
  as an exception, and other threads were able to run throughout.
- **M4.** An FFmpeg call that can reach a callback of §7.1 is made with the
  lock released.

Which of the remaining operations release the lock is not specified.

### 6.2 Use of one object from several threads

A program may run several threads and several domains.

- **M5. Stateful handles are memory-safe under any interleaving.** For a
  handle with a guard (M6), no interleaving of operations, from any number of
  threads or domains, may corrupt memory or crash.
- **M6. Use guard.** Containers, decoders, encoders, bitstream filter
  instances, filter graphs, resamplers and scalers each have a guard. An
  operation that changes the object's structure or state takes it
  exclusively; an operation that only reads takes it shared. An operation
  that finds the guard taken in a conflicting way raises the in-use error of
  §5.3 at once. It does not wait.
- **M7.** The guard is held for one native operation, including the time the
  runtime lock is released and any callback of §7.1 that runs inside it. It
  is not held across a composite operation of §11, nor while a synchronous
  user function of §7.2 runs.
- **M8. Frames and packets have no guard.** They are passed around on hot
  paths and shared between consumers, where a guard would cost on every
  access. Any number of threads may read one at the same time, including
  giving it to codecs, filters, muxers and converters. A thread that changes
  one, through a setter or by making a frame writable, MUST be its only
  user at that moment; the binding does not detect a violation.
- **M9.** A dependent handle shares its owner's guard.
- **M12. The guard must not be felt.** Taking and releasing a guard costs a
  constant, small number of uncontended atomic operations and never blocks.
  An implementation whose guards measurably slow an encoding or decoding
  loop does not conform.

Each library's §6 lists, per guarded handle, which operations are exclusive
and which are shared.

### 6.3 Threads FFmpeg creates

- **M10.** A C function that FFmpeg may call on a thread unknown to the OCaml
  runtime registers that thread before touching OCaml state, and the
  registration is undone when the thread exits ([avutil.md](avutil.md) §6.3).
- **M11.** FFmpeg's worker threads enter OCaml only through the callbacks of
  §7.1.

### 6.4 Global state

Each library's §6 lists what it initialises at load time and any process-wide
state it keeps. Process-wide state MUST be safe to reach from several domains.

## 7. Callbacks

### 7.1 Functions FFmpeg calls

A user closure that FFmpeg invokes during one of its own calls (custom I/O,
interruption, device messages) is called through this sequence:

1. Register the current thread (M10).
2. Acquire the runtime lock.
3. Call the closure, catching any exception.
4. Translate the result to the C return value FFmpeg's callback contract
   expects.
5. Release the runtime lock.

- **C1.** An exception never unwinds through FFmpeg. It becomes the error
  return each library states, and its text is written to FFmpeg's log at
  error level.
- **C2.** A result the closure returns outside the range its contract allows
  is treated as a failure of the closure.
- **C3.** The closure is kept alive from installation until the object is
  released or the closure is replaced, and no longer (L8).
- **C4.** A closure of this kind runs while the operation that triggered it
  holds the object's guard: an operation on that object called from the
  closure raises the in-use error (M7). A closure that a thread of FFmpeg's
  own invokes outside any operation finds the guard free or taken, as it
  happens.

Each library's §7 states, per callback: what triggers it, on which thread it
may run, the value an exception turns into.

Log messages are different: FFmpeg's log callback never enters OCaml
([avutil.md](avutil.md) §7.1).

### 7.2 Functions the binding calls

A user function passed to an operation and called by the binding itself,
between native calls, runs on the caller's thread with the runtime lock held.

- **C5.** Its exceptions propagate to the caller of the operation (F5).
- **C6.** The object is not in use while it runs (M7): it may call any
  operation on the object.
- **C7.** Each library's §7 states what state the object is in when the
  function is called and after it raises.

## 8. Data transfer

For each path that moves media data or text across the boundary, the library
states whether the bytes are **copied** or **shared**.

- **S1.** OCaml strings and byte sequences are copied in both directions.
- **S2.** A bigarray MAY share memory with a native buffer. It then keeps the
  buffer alive (L4), and its length is exactly the number of elements the
  buffer validly holds for that plane or channel.
- **S3.** A value built by an operation for its caller is not written again by
  the binding after the operation returns.
- **S4.** Frames and packets are passed to FFmpeg by reference (A4).

Plane counts, line sizes, alignment and sample layout are stated per path.

## 9. Options

[avutil.md](avutil.md) §9 owns the option-table protocol. In short:

- **O1.** An operation that takes `?opts` hands the caller's entries to
  FFmpeg. After a successful call the caller's table holds exactly the
  entries FFmpeg did not consume.
- **O2.** A typed argument of an operation MUST take effect on the native
  object, or the operation fails. It is never turned into an entry that FFmpeg
  may ignore.
- **O3.** Each library's §9 lists, per operation, which FFmpeg objects consume
  the entries and in which order.

## 10. Version-dependent behaviour

[compatibility.md](compatibility.md) owns the supported range and the rules
for conditionals. Each library's §10 lists what differs between supported
FFmpeg versions; nothing does.

## 11. Composite operations

Operations defined entirely in terms of other operations of the interface:
drain loops, best-value selection, converters built on a private graph. Each
library's §11 gives their algorithm. A composite operation is not atomic with
respect to other threads (M7).
