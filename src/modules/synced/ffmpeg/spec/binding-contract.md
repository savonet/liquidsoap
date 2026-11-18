# Binding contract

What the seven FFmpeg binding libraries add to the
[generic binding contract](../../spec/binding-contract.md), which owns every
rule this file links to; §1 to §8 have the generic contract's numbers. Each
library has its own file with the same eleven sections in the same order, so
"§6" is the same topic everywhere. A library file answers every clause of this contract for its
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
   workspace ([cross-compilation.md](cross-compilation.md)), with the
   environment variables of the generic
   [cross-compilation.md §3.2](../../spec/cross-compilation.md#32-embedded-in-a-workspace).
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

The kinds are those of the [generic contract §2.1](../../spec/binding-contract.md#21-kinds-of-handle).
Borrowed handles point to static FFmpeg data: a codec, a format, an option
class, a pixel-format descriptor. Dependent handles designate a stream of a
container, a filter of a graph, a plane of a frame, an option-bearing object
inside a container.

Each library's §2 lists its handles with: the native object, how the handle
is created, what it owns, what it keeps alive, how it is released, and its
states.

### 2.2 Lifetime rules

L1 to L10 are the [generic contract's](../../spec/binding-contract.md#22-lifetime-rules). FFmpeg
additions:

- **L7.** Release by collection flushes no encoder and writes no trailer.
- **L9.** A handle on a frame or a packet MUST report the size of the native
  memory it holds to the garbage collector, whichever operation produced it.
- **L10.** The allocator is FFmpeg's.

### 2.3 Boundary rules

B1 to B3 are the [generic contract's](../../spec/binding-contract.md#23-boundary-rules). The phantom type
parameters of B3 carry the media kind, the direction and the mode; each
library's §2 lists the places where B3 applies.

## 3. Enumerations and constants

FFmpeg enumerations are OCaml polymorphic variants. Two sources:

- **Generated tables**, derived from the target's FFmpeg headers
  ([build.md](build.md) §3). The OCaml type and both conversion directions
  come from the same table.
- **Hand-written tables**, for small flag sets and for enumerations the
  generator does not cover. Each library lists them in full in its §3.

Every table follows E1 to E7 of the
[generic contract §3](../../spec/binding-contract.md#3-enumeration-tables). The failure error of E3 is
``Error (`Failure msg)``.

## 4. Operations

Each library's §4 gives every item of its interface, in the interface's
order, with its signature and its behaviour. A1 to A3 and A5 to A7 are the
[generic contract's](../../spec/binding-contract.md#4-operations); the failure error of A1 is
``Error (`Failure msg)``, and a plane shape is one of the shapes A1 checks.

- **A4. Inputs are not consumed.** An operation that hands a frame or a packet
  to FFmpeg leaves the caller's value unchanged and usable. The binding gives
  FFmpeg a reference of its own.

## 5. Errors

### 5.1 One exception

`Avutil.Error` carries every failure of the bindings. [avutil.md](avutil.md)
§5 owns its definition and the mapping from FFmpeg error codes.

- **F1.** A negative return from an FFmpeg call raises `Error` with the mapped
  code.
- **F2.** A condition the binding detects itself raises
  ``Error (`Failure msg)``. `msg` is for a human reader; its wording is not
  part of the contract except for the texts of §5.3.
- **F3**, **F4**, **F5**: the [generic contract §5.1](../../spec/binding-contract.md#51-exceptions).
  FFmpeg's out-of-memory code is raised as `Error` with that code: FFmpeg
  uses it for more than memory, such as a hardware device it cannot create.
  The lookups of F4 are listed in each library's §5. `Avfilter.Exists` is the
  one exception a library defines besides `Avutil.Error`.

### 5.2 State after a failure

- **F6**: the [generic contract §5.2](../../spec/binding-contract.md#52-state-after-a-failure), with the
  states of each library's §2.
- **F7.** A failure of an operation that takes `?opts` leaves the caller's
  option table untouched.

### 5.3 State errors

The state errors of the [generic contract §5.3](../../spec/binding-contract.md#53-state-errors):

| State                                            | Raised value                             |
| ------------------------------------------------ | ---------------------------------------- |
| the handle, or its owner, is closed              | ``Error (`Failure "Container closed!")`` |
| the handle, or its owner, is failed              | ``Error (`Failure "Object failed!")``    |
| the handle is in use by another operation (§6.2) | ``Error (`Failure "Object in use!")``    |

These three texts are part of the contract.

## 6. Blocking and concurrency

### 6.1 The runtime lock

M1 to M4 are the [generic contract's](../../spec/binding-contract.md#61-the-runtime-lock). The
operations of M1 are those that wait for input or output, open or probe a
container, open a codec, send to or receive from a codec or a filter,
configure or run a filter graph, convert media data, or initialise or
transfer to a hardware context.

### 6.2 Use of one object from several threads

M5 to M7, M9 and M12 are the
[generic contract's](../../spec/binding-contract.md#62-use-of-one-object-from-several-threads).
Containers, decoders, encoders, bitstream filter instances, filter graphs,
resamplers and scalers each have a guard (M6). The composite operations of M7
are those of §11. M12's hot loops are encoding and decoding loops.

- **M8. Frames and packets have no guard.** They are passed around on hot
  paths and shared between consumers, where a guard would cost on every
  access. Any number of threads may read one at the same time, including
  giving it to codecs, filters, muxers and converters. A thread that changes
  one, through a setter or by making a frame writable, MUST be its only
  user at that moment; the binding does not detect a violation.

Each library's §6 lists, per guarded handle, which operations are exclusive
and which are shared.

### 6.3 Threads FFmpeg creates

M10 and M11 are the
[generic contract's](../../spec/binding-contract.md#63-threads-the-c-library-creates). FFmpeg's
registration is [avutil.md](avutil.md) §6.3.

### 6.4 Global state

The [generic contract §6.4](../../spec/binding-contract.md#64-global-state); each library's §6 lists its
own.

## 7. Callbacks

### 7.1 Functions FFmpeg calls

The callbacks FFmpeg invokes during its own calls (custom I/O, interruption)
follow the sequence and C1 to C4 of the
[generic contract §7.1](../../spec/binding-contract.md#71-functions-the-c-library-calls). FFmpeg
addition to C1: the text of the exception is written to FFmpeg's log at error
level. Each library's §7 states, per callback, what triggers it, on which
thread it may run, and the value an exception turns into.

Log messages are different: FFmpeg's log callback never enters OCaml
([avutil.md](avutil.md) §7.1).

### 7.2 Functions the binding calls

C5 to C7 are the [generic contract's](../../spec/binding-contract.md#72-functions-the-binding-calls); each
library's §7 answers C7.

## 8. Data transfer

For each path that moves media data or text across the boundary, the library
states whether the bytes are **copied** or **shared**.

S1 to S3 are the [generic contract's](../../spec/binding-contract.md#8-data-transfer); the buffer of S2
is a plane or a channel.

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

[compatibility.md](compatibility.md) owns the supported range; the rules for
conditionals are the generic
[compatibility.md §2](../../spec/compatibility.md#2-version-dependent-behaviour). Each library's §10 lists what differs between supported
FFmpeg versions; nothing does.

## 11. Composite operations

Operations defined entirely in terms of other operations of the interface:
drain loops, best-value selection, converters built on a private graph. Each
library's §11 gives their algorithm. A composite operation is not atomic with
respect to other threads (M7).
