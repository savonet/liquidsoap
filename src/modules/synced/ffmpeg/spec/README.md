# OCaml FFmpeg bindings — specification

What the bindings must do, written so that they can be implemented from this
text, the generic level above it and FFmpeg's own documentation alone.

The key words MUST, SHOULD and MAY are used as in RFC 2119.

This tree is the FFmpeg level of the
[synced C bindings specification](../../spec/README.md), itself under the
[project specifications](../../../../spec/README.md). The rules every synced
binding shares (handles and lifetime, the runtime lock and the use guard,
callbacks, detection, generated code, cross-compilation, version and test
rules) are owned there; the files of this tree answer them for FFmpeg, link
to them, and own only what is FFmpeg's.

## Scope

In: the seven binding libraries (`avutil`, `avcodec`, `av`, `avfilter`,
`avdevice`, `swresample`, `swscale`), the build-time availability detection
and enumeration generation, cross-compilation, and the conformance
requirements of a test suite.

Out: example programs, and any application built on the bindings.

The reader is someone implementing these bindings in OCaml and C against
FFmpeg's C API.

## Reading order

| #   | Document                                     | Owns                                                                                                        |
| --- | -------------------------------------------- | ----------------------------------------------------------------------------------------------------------- |
| 1   | [binding-contract.md](binding-contract.md)   | What the seven libraries add to the generic contract, the frozen surface, the eleven-section layout         |
| 2   | [compatibility.md](compatibility.md)         | The FFmpeg floor and ceiling, the OCaml minimum, what the floor guarantees                                  |
| 3   | [build.md](build.md)                         | Packages, FFmpeg's detection requirements, enumeration tables, installed C headers, how the tests are wired |
| 4   | [cross-compilation.md](cross-compilation.md) | What FFmpeg adds to the generic Windows build                                                               |
| 5   | [avutil.md](avutil.md)                       | The base library: errors, logging, frames, channel layouts, formats, options, hardware contexts             |
| 6   | [avcodec.md](avcodec.md)                     | Codecs, packets, parameters, encoders, decoders, bitstream filters                                          |
| 7   | [avformat.md](avformat.md)                   | The `av` library: containers, streams, custom I/O, demuxing, muxing                                         |
| 8   | [avfilter.md](avfilter.md)                   | Filter graphs                                                                                               |
| 9   | [avdevice.md](avdevice.md)                   | Registration of capture and playback devices                                                                |
| 10  | [swresample.md](swresample.md)               | Audio resampling and sample-format conversion                                                               |
| 11  | [swscale.md](swscale.md)                     | Image scaling and pixel-format conversion                                                                   |
| 12  | [side-data.md](side-data.md)                 | Side data across the libraries: carriers, raw entries, the three levels of the interface                    |
| 13  | [tests.md](tests.md)                         | Conformance: what a test suite must assert and what it must not depend on                                   |
| 14  | [known-complexity.md](known-complexity.md)   | Pitfalls these bindings have already paid for, each with a check and the rule that answers it               |

Every rule has one owner. Other files link to it.

Build in this order, each layer with its conformance requirements before the
next: build and detection, `avutil`, `avcodec`, `av`, then `avfilter`,
`avdevice`, `swresample` and `swscale` in any order.

## Two parts

**The specification** is the files above. It is normative.

**The language notes** are advice about the implementation language, its
build system and FFmpeg's API. They are not normative and describe no design
to copy.

| Note                                                                   | About                                                   |
| ---------------------------------------------------------------------- | ------------------------------------------------------- |
| [ocaml-c-interface.md](../../spec/language-notes/ocaml-c-interface.md) | writing C stubs for OCaml 5 (generic)                   |
| [build-system.md](../../spec/language-notes/build-system.md)           | dune, detection, generated code, cross builds (generic) |
| [ffmpeg-api.md](language-notes/ffmpeg-api.md)                          | behaviour of FFmpeg's API that its headers omit         |

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
11. Composite operations

§4 of each library file is that library's whole OCaml interface: every type,
value and module with its signature.

## Rule identifiers

Rules carry a letter and a number, unique within a file. A rule that moved
to the [generic level](../../spec/README.md#rule-identifiers) kept its
identifier there, and the file of this tree that held it keeps a line
linking to it, with any FFmpeg addition:

| File                   | Letters                                                                                                                                  |
| ---------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| `binding-contract.md`  | I initialisation, L lifetime, B boundary, E enumerations, A operations, F errors, M concurrency, C callbacks, S data transfer, O options |
| `compatibility.md`     | V                                                                                                                                        |
| `build.md`             | P packages, D detection, G generation, H headers, T tests, K packaging                                                                   |
| `cross-compilation.md` | X                                                                                                                                        |
| `avutil.md`            | N logging                                                                                                                                |
| `side-data.md`         | R                                                                                                                                        |
| `tests.md`             | H harness; requirements are numbered `section.item`                                                                                      |

"Contract L3" means rule L3 of `binding-contract.md`, here or, for a rule
that moved, at the generic level.

## Documents that were considered and not written

- **Error model.** One exception and one table, owned by
  [avutil.md](avutil.md) §5, with the shared rules in the contract's §5.
- **Threading model.** The contract's §6 and §7, with the per-handle detail in
  each library's §6.
- **Media data layouts.** They are specific to the conversion paths that use
  them: [swresample.md](swresample.md) §3.2, [swscale.md](swscale.md) §3.2 and
  [avutil.md](avutil.md) §8.
- **Option handling.** One protocol, owned by [avutil.md](avutil.md) §9.

## Not verified

The specification was derived from a description of an earlier
implementation, which was exercised on Linux aarch64 with OCaml 5.5.0 against
FFmpeg 7.1, 8.1 and 9.0. Nothing below was run by anyone:

- any build or run on Windows, any cross build, and macOS;
- hardware encoding, hardware devices and hardware frame pools;
- any capture or playback device;
- an OCaml version other than 5.5.0, and a program with several domains;
- the resampler's alternative engine, absent from the FFmpeg builds used.

Rules that rest on those, and on statements about FFmpeg marked
**unverified** in [language-notes/ffmpeg-api.md](language-notes/ffmpeg-api.md),
are to be confirmed by the conformance suite.
