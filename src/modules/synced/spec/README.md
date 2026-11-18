# Synced C bindings — specification

What every OCaml binding to a C library in `src/modules/synced/` MUST,
SHOULD and MAY do. It sits between the project level
([src/spec/README.md](../../../spec/README.md)) and the specification of each
binding, and follows [conventions.md](../../../spec/conventions.md).

## Scope

In: every binding vendored in `src/modules/synced/` that has C stubs (alsa,
ao, dssi, fdkaac, ffmpeg, frei0r, jack, ladspa, lame, lilv, lo, mad,
portaudio, pulseaudio, samplerate, shine, soundtouch, srt, xiph and the
others): native handles and their lifetime, the boundary between the OCaml
runtime and C, errors, the runtime lock and concurrent use, callbacks, data
transfer, enumeration tables, availability detection, generated code,
installed headers, cross-compilation, version ranges, and the run
configurations of a conformance suite.

Out, and owned elsewhere:

- what a binding binds, its OCaml interface, its supported version range and
  its own tables: the binding's own tree. Only
  [ffmpeg/spec](../ffmpeg/spec/README.md) exists today;
- what Liquidsoap does with a binding: the component trees of
  [src/spec/README.md](../../../spec/README.md);
- what every conformance suite does:
  [testing.md](../../../spec/testing.md); resources and shared state:
  [concurrency.md](../../../spec/concurrency.md).

A binding's tree answers each rule here that applies to it, links to it, and
states a departure next to the rule it departs from.

## Reading order

| #   | Document                                     | Owns                                                                                                                                       |
| --- | -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| 1   | [binding-contract.md](binding-contract.md)   | Handles, lifetime, the boundary, enumeration tables, arguments, errors, the runtime lock, the use guard, threads, callbacks, data transfer |
| 2   | [compatibility.md](compatibility.md)         | Floor and ceiling, version conditionals, the OCaml range                                                                                   |
| 3   | [build.md](build.md)                         | Availability detection, flags, gating, the install check, warnings, generated code, installed headers                                      |
| 4   | [cross-compilation.md](cross-compilation.md) | Host and target contexts, build-time programs, per-context pkg-config, what stubs may use, the two Windows invocations                     |
| 5   | [tests.md](tests.md)                         | What a binding's suite adds to [testing.md](../../../spec/testing.md): versions, run configurations, build checks                          |

Build in this order: detection and the build first, then the contract,
checked by the run configurations of [tests.md](tests.md).

## Two parts

The files above are Part A. Part B is in [language-notes/](language-notes/):

| Note                                                        | About                                         |
| ----------------------------------------------------------- | --------------------------------------------- |
| [ocaml-c-interface.md](language-notes/ocaml-c-interface.md) | writing C stubs for OCaml 5                   |
| [build-system.md](language-notes/build-system.md)           | dune, detection, generated code, cross builds |

## Rule identifiers

Rules carry a letter and a number, unique within a file. A binding's tree
uses the same identifiers for the same rules: "contract L3" is rule L3 of
[binding-contract.md](binding-contract.md) unless the binding's own contract
defines an L3.

| File                   | Letters                                                                                            |
| ---------------------- | -------------------------------------------------------------------------------------------------- |
| `binding-contract.md`  | L lifetime, B boundary, E enumerations, A operations, F errors, M concurrency, C callbacks, S data |
| `compatibility.md`     | V                                                                                                  |
| `build.md`             | D detection, G generation, H headers                                                               |
| `cross-compilation.md` | X                                                                                                  |
| `tests.md`             | H harness                                                                                          |

## Vocabulary

- **C library**: the native library a binding binds, found through
  pkg-config.
- **Binding library**: one OCaml library of a binding, with its C stubs. A
  binding has one or several.
- **Owned handle**: an OCaml value that owns a native object. The object is
  released when the value is collected, or earlier by an explicit close where
  the interface has one.
- **Borrowed handle**: an OCaml value holding a pointer to static data of the
  C library, valid for the life of the process and never released.
- **Dependent handle**: a value designating part of another object, its
  **owner**.
- **Closed**, **failed**: states of a handle after an explicit close, and
  after a failure its binding defines as leaving the object unusable.
- **Guard**: the per-object marker that detects concurrent use
  ([binding-contract.md §6.2](binding-contract.md#62-use-of-one-object-from-several-threads)).
  An object is **in use** while an operation holds its guard.
- **Runtime lock**: the OCaml runtime lock of the calling domain.
- **Available**: a binding library whose detection succeeded in a context
  ([build.md §2](build.md#2-availability-detection)).
- **Host context**, **target context**: the build context whose programs run
  on the build machine, and the one whose libraries ship
  ([cross-compilation.md §1](cross-compilation.md#1-model)).
- **Build-time program**: a program a build rule runs (detection, a
  generator). It is a host program.
- **Floor**: the oldest supported release of a C library. **Ceiling**: the
  newest release the conformance suite runs against.

## Compatibility surface

Beyond the defaults of [conventions.md §5](../../../spec/conventions.md#5-compatibility-surface):
each binding's opam package and library names, its installed C header file
names, and these build environment variables, read by the detection of every
synced binding. Their names and meanings change only as a confirmed change.

| Variable                              | Meaning                                                                                       |
| ------------------------------------- | --------------------------------------------------------------------------------------------- |
| `LIQUIDSOAP_DUNE_TARGET`              | Replaces the context name in pkg-config selection, in every context (cross-compilation X7).   |
| `PKG_CONFIG_PATH_<ctx>`               | pkg-config search path for context `<ctx>`, the context name with `.` replaced by `_` (X4).   |
| `PKG_CONFIG_<ctx>`                    | pkg-config executable for context `<ctx>` (X4).                                               |
| `PKG_CONFIG`, `PKG_CONFIG_PATH`       | Ambient pkg-config executable and search path, used when no per-context variable is set (X5). |
| `LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS`     | Space-separated list of binding library names. A listed library is unavailable (build D2).    |
| `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL` | When equal to `true`, disables the install-time check (build D13).                            |

## Concerns without a document

- **Error model.** Each binding defines its exceptions; the shared rules are
  the contract's §5.
- **Options and composite operations.** Only FFmpeg has them; its tree owns
  them.

## Open

Tensions found while extracting the generic level. None is settled here.

- **`_tlv_atexit` and what stubs may use.** Cross-compilation X12 and X13
  allow only ISO C, the OCaml runtime, the C library's headers and POSIX
  threads. [language-notes/ocaml-c-interface.md](language-notes/ocaml-c-interface.md)
  recommends `_tlv_atexit`, a macOS dyld function, for thread unregistration
  (contract M10), and the FFmpeg stubs call it under `__APPLE__`. Either X12
  and X13 are scoped to the Windows target, or they admit a named
  platform exception, or M10 is met another way on macOS.
- **The use guard outside FFmpeg.** Contract M5 to M12 hold for every
  binding; only the FFmpeg stubs implement a guard today.
- **OCaml floors.** Compatibility V9 asks for correctness with several
  domains; several bindings' opam files state OCaml floors from 4.02 to 5.1.
- **Detection without pkg-config.** Build D2 and D3 take availability and
  flags from pkg-config; some detection programs (lame) fall back to a
  compile test with default flags when pkg-config has no entry.
- **Build-time programs with C stubs.** Cross-compilation X2 and X15; some
  detection programs (lame) link a C stub that inspects the build machine.
- **The Windows link-flag filter** (build D7) is implemented by the ffmpeg
  and srt detection only.
- **"Implemented once"** (cross-compilation X6) holds within each binding;
  every binding carries its own copy of the selection.
