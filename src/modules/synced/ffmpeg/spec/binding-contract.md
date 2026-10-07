# Binding contract

What the seven binding libraries have in common: the questions each one
answers and the conventions they share. Each library has its own file with
the same eleven sections in the same order, so "§6" is the same topic
everywhere.

| Library      | C libraries bound                                     | File                           |
| ------------ | ----------------------------------------------------- | ------------------------------ |
| `avutil`     | libavutil (and libavcodec for the subtitle structure) | [avutil.md](avutil.md)         |
| `avcodec`    | libavcodec                                            | [avcodec.md](avcodec.md)       |
| `av`         | libavformat                                           | [avformat.md](avformat.md)     |
| `avfilter`   | libavfilter                                           | [avfilter.md](avfilter.md)     |
| `avdevice`   | libavdevice                                           | [avdevice.md](avdevice.md)     |
| `swresample` | libswresample                                         | [swresample.md](swresample.md) |
| `swscale`    | libswscale                                            | [swscale.md](swscale.md)       |

This file describes the conventions as the code follows them today. Where one
library departs from a convention, its own file says so in the matching
section, and [findings.md](findings.md) lists the departure.

## 1. Scope

Each library is one OCaml module with one C stub file, built only when its C
libraries are detected ([build.md](build.md) §2).

Dependencies between the binding libraries:

    avutil  <-  avcodec  <-  av  <-  avdevice
       ^           ^
       |           +---  swresample
       +---  avfilter
       +---  swscale

`avutil`, `avcodec` and `av` each install a C header. A dependent library's
stubs include it to reach the accessors and constructors of the objects
defined there. `avutil`'s header is the shared base: error raising, option
dictionaries, thread registration, rationals, time formats, enum conversions,
and the frame, subtitle, channel-layout and hardware-context wrappers
([avutil.md](avutil.md) §1.4).

A library's own sources state no minimum FFmpeg version. The bound is set by
detection and owned by [compatibility.md](compatibility.md) §1.1.

Loading a module runs its initialisation, which may fail
([avutil.md](avutil.md) §1.3 for the base).

## 2. Objects

Every OCaml-visible type that stands for a C object is one of three kinds.

**Owned handle.** The OCaml value owns the C object. Garbage collection of the
value releases the C object. The handle records an estimate of the C memory it
holds so the collector accounts for it where the code does so (frames and
packets). Some owned handles also have an explicit close operation; after it,
the handle remains a valid OCaml value and each operation states what it does
on a closed handle.

**Borrowed pointer.** The OCaml value holds a pointer it does not own and never
frees. Two sub-cases:

- the pointer refers to static FFmpeg data (a codec descriptor, an option
  class, a format descriptor, a pixel-format descriptor) and is valid for the
  life of the process;
- the pointer refers into another object (an option-bearing object inside a
  codec or filter context). The OCaml value then pairs the pointer with the
  OCaml value of the owner, which keeps the owner alive.

**Dependent handle.** An object that is meaningful only while its parent
exists: a stream of a container, a filter of a graph. Each library states
what, if anything, keeps the parent alive while the dependent handle is
reachable, and what an operation on the dependent handle does after the
parent is closed.

Phantom type parameters carry the media kind (`audio`, `video`, `subtitle`),
the direction (`input`, `output`) and the mode (raw frames or encoded
packets). They exist only in the types. The stubs trust them: a value built
with the wrong parameter is not detected.

## 3. Enumerations and constants

FFmpeg enumerations are OCaml polymorphic variants. Two sources:

- **Generated tables**, built from the installed FFmpeg headers
  ([build.md](build.md) §3). The OCaml type and both conversion directions
  come from the same table.
- **Hand-written tables** in the stubs, for small flag sets and for
  enumerations the generator does not cover. Each library lists them in full.

A conversion with no entry for its input raises ``Error (`Failure msg)`` with
the message given in [avutil.md](avutil.md) §5.2, in both directions.

## 4. Operations

Every item of the library's interface, in the interface's order, with its
signature, its steps, the FFmpeg calls it makes and its failure behaviour.

## 5. Errors

One exception carries every FFmpeg failure: `Avutil.Error`. [avutil.md](avutil.md)
§5 owns its definition, the table from FFmpeg error codes to its
constructors, and the rules for failures raised by the binding itself.

Conventions:

- A negative return from an FFmpeg call raises `Error` with the mapped code.
- A condition the binding detects itself raises ``Error (`Failure msg)``.
- A failed allocation raises `Out_of_memory`.
- `Not_found` and `Invalid_argument` are raised by a small number of lookups;
  each library lists them.

Each library states what it releases on each failure path and what state its
objects are left in.

## 6. Blocking and concurrency

**Runtime lock.** A stub that calls an FFmpeg function which may block or run
for long (I/O, codec open, encode, decode, filter, convert) releases the
OCaml runtime lock around that call, so other OCaml threads run meanwhile.
Each library lists exactly which calls do. Everything not listed holds the
lock for its whole duration.

**No per-object lock.** No object is protected against concurrent use. While
one thread is inside an operation with the runtime lock released, another
thread can enter any operation on the same object.

**Thread registration.** A C function that FFmpeg may call on a thread unknown
to the OCaml runtime registers that thread before touching OCaml state
([avutil.md](avutil.md) §6.3).

**Global state.** Each library lists what it initialises at load time and any
process-wide state it keeps.

## 7. Callbacks

A C function that FFmpeg calls and that runs an OCaml closure follows one
sequence:

1. Register the current thread.
2. Acquire the runtime lock.
3. Call the OCaml closure, catching any exception.
4. Translate the result to the C return value the FFmpeg callback contract
   expects. An exception becomes an error return; it does not propagate
   through FFmpeg frames.
5. Release the runtime lock.

The closure is kept alive by the object it was installed on and released with
that object.

Each library states, per callback: what triggers it, on which thread, the
value an exception turns into, and whether the exception is reported anywhere.

Log messages are the exception: FFmpeg's log callback never enters OCaml. It
queues the message, and an OCaml thread delivers it later
([avutil.md](avutil.md) §7.1).

Functions passed to OCaml-side iteration helpers are ordinary OCaml calls:
they run on the caller's thread with the lock held, and their exceptions
propagate.

## 8. Data transfer

For each path that moves media data or strings across the boundary, the
library states whether the bytes are **copied** or **shared**:

- OCaml strings and byte sequences are always copied in both directions.
- Bigarrays may share memory with a C buffer. When they do, the library
  states what keeps the C buffer alive while the bigarray is reachable.
- Frames and packets are passed to FFmpeg by pointer. Whether FFmpeg takes its
  own reference or takes over the caller's, leaving the OCaml value empty, is
  stated per operation.

Plane counts, line sizes, alignment and sample layout are stated per path.

## 9. Options

Operations that accept `?opts` follow the option-dictionary protocol owned by
[avutil.md](avutil.md) §9.1: the caller's table is rendered to a dictionary,
FFmpeg consumes the entries it recognises, and the caller's table is then
pruned to the entries that were left. A key still in the table after the call
was not used.

Introspection of FFmpeg's `AVOption` system (listing an object's options,
reading their current values) is owned by [avutil.md](avutil.md) §4.15–4.17
and §9.3–9.4. Other libraries supply the option class and the object to read.

## 10. Version-dependent behaviour

Every difference in behaviour between FFmpeg versions.
[compatibility.md](compatibility.md) owns the supported range and the list of
conditionals; each library describes the behaviour on each side.

## 11. Logic on the OCaml side

Algorithms that live in OCaml code rather than in the stubs: iteration
helpers, functors, state the OCaml side tracks, defaults it computes.
