# OCaml FFmpeg bindings — as-built specification

What the bindings in this directory do today, written so that they can be
rebuilt without reading their source.

This is a snapshot, not a design. It describes the behaviour of version 1.4.0
as it is, defects included. What is wrong with that behaviour is kept apart,
in [findings.md](findings.md).

## Scope

In: the seven binding libraries (`avutil`, `avcodec`, `av`, `avfilter`,
`avdevice`, `swresample`, `swscale`) with their C stubs, the build-time
availability detection and enum-table generator, and the test suite described
by principle.

Out: the example programs, except where the test suite runs them, and
liquidsoap's use of the bindings.

The reader is someone rewriting these bindings in OCaml against the same
FFmpeg C API.

## Reading order

| #   | Document                                     | Owns                                                                                                                                        |
| --- | -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | [binding-contract.md](binding-contract.md)   | The conventions all seven libraries share, and the eleven-section layout every library file follows                                         |
| 2   | [compatibility.md](compatibility.md)         | The supported FFmpeg and OCaml versions, every version conditional, library versions per FFmpeg release                                     |
| 3   | [build.md](build.md)                         | Packages, availability detection, the enum-table generator and its output formats, how tests and examples are wired                         |
| 4   | [cross-compilation.md](cross-compilation.md) | Building for Windows with `dune -x windows` and opam-cross-windows                                                                          |
| 5   | [avutil.md](avutil.md)                       | The base library: errors, logging, frames, channel layouts, formats, options, hardware contexts, and the C helpers the other stubs build on |
| 6   | [avcodec.md](avcodec.md)                     | Codecs, packets, parameters, encoders, decoders, bitstream filters                                                                          |
| 7   | [avformat.md](avformat.md)                   | The `av` library: containers, streams, custom I/O, demuxing, muxing                                                                         |
| 8   | [avfilter.md](avfilter.md)                   | Filter graphs                                                                                                                               |
| 9   | [avdevice.md](avdevice.md)                   | Capture and playback devices, control messages                                                                                              |
| 10  | [swresample.md](swresample.md)               | Audio resampling and sample-format conversion                                                                                               |
| 11  | [swscale.md](swscale.md)                     | Image scaling and pixel-format conversion                                                                                                   |
| 12  | [tests.md](tests.md)                         | The test suite by principle: invariants, what is binding, what is incidental, the coverage map                                              |
| 13  | [findings.md](findings.md)                   | Defects, asymmetries, gaps and API gaps, ranked; what was never run                                                                         |

Every rule has one owner. Other files link to it.

## Two parts

**Part A** is the files above: objects, ownership, lifetimes, operations,
errors, blocking, callbacks, data transfer, in terms a user of the interface
or an auditor of its resources can observe.

**Part B** is [language-notes/](language-notes/), one file per subsystem: how
the current stubs achieve it. Custom block layouts, root discipline,
registered values, macros, build-system idioms. It is advice about the
implementation language, not part of the contract.

## Section numbers

Every library file uses the same sections, defined in
[binding-contract.md](binding-contract.md):

1. Scope
2. Objects
3. Enumerations and constants
4. Operations
5. Errors
6. Blocking and concurrency
7. Callbacks
8. Data transfer
9. Options
10. Version-dependent behaviour
11. Logic on the OCaml side

## Documents that were considered and not written

- **Error model.** One exception and one table, owned by
  [avutil.md](avutil.md) §5. It needs no file of its own.
- **Threading model.** Three rules, stated once in
  [binding-contract.md](binding-contract.md) §6 and §7, with the per-call
  detail in each library's §6.
- **Media data layouts.** Sample and plane layouts are specific to the
  conversion paths that use them and stay in [swresample.md](swresample.md)
  §8, [swscale.md](swscale.md) §8 and [avutil.md](avutil.md) §8.
- **Option handling.** One protocol, owned by [avutil.md](avutil.md) §9.

## Constraints set for the next pass

These were decided while this snapshot was written. They are not part of the
as-built description; they frame what is done with it.

1. **The interface is frozen.** The `.mli` files keep their current API unless
   a logical gap is identified. The candidates are in
   [findings.md](findings.md) §3.
2. **FFmpeg range.** The targets are FFmpeg 8 and 9, and 7 where it makes
   sense. [compatibility.md](compatibility.md) §4 shows what each floor costs:
   from 7.1 onward every version conditional takes one side; 7.0 keeps one.
3. **OCaml range.** OCaml 5.5 may become the minimum where that simplifies
   the implementation.
4. **Cross-compilation stays.** The bindings must keep building for Windows
   through opam-cross-windows and `dune -x windows`, standalone and embedded
   in liquidsoap.
