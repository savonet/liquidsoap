# FFmpeg and OCaml version compatibility

The range of FFmpeg and OCaml versions the bindings build and run against, and
the rules for behaviour that depends on a version.

## 1. FFmpeg range

### 1.1 Floor

- **V1.** The oldest supported release is **FFmpeg 7.1**. The targets are
  FFmpeg 8 and FFmpeg 9.
- **V2.** The floor is enforced by detection: a binding library is built only
  when pkg-config reports its C libraries at the versions they have in the
  floor release. [build.md](build.md) §2.3 owns the table.
- **V3.** The floor stated to users, the floor detection enforces and the
  oldest release the conformance suite runs against MUST be the same release
  ([tests.md](tests.md) §8).

A floor that detection accepts and nobody builds is found by users, in the
middle of an install.

### 1.2 Ceiling

There is no upper bound in the build. The newest release the conformance
suite runs against is the newest supported release.

### 1.3 Library versions per release

| Release | libavutil | libavcodec | libavformat | libavfilter | libavdevice | libswscale | libswresample |
| ------- | --------- | ---------- | ----------- | ----------- | ----------- | ---------- | ------------- |
| 7.1     | 59.39.100 | 61.19.100  | 61.7.100    | 10.4.100    | 61.3.100    | 8.3.100    | 5.3.100       |
| 8.0     | 60.8.100  | 62.11.100  | 62.3.100    | 11.4.100    | 62.1.100    | 9.1.100    | 6.1.100       |
| 8.1     | 60.26.100 | 62.28.100  | 62.12.100   | 11.14.100   | 62.3.100    | 9.5.100    | 6.3.100       |
| 9.0     | 61.1.100  | 63.1.100   | 63.1.100    | 12.1.100    | 63.1.100    | 10.1.100   | 7.1.100       |

## 2. Version-dependent behaviour

- **V4.** Across the supported range the bindings have no behaviour selected
  by an FFmpeg version. Each library file's §10 states this for its library,
  or lists what differs.

What the floor guarantees, and the bindings rely on without a conditional:

| Facility                                                                | Used by                         |
| ----------------------------------------------------------------------- | ------------------------------- |
| channel layouts as structures (`AVChannelLayout`), no integer masks     | every library                   |
| a duration field on frames                                              | [avutil.md](avutil.md) §4.3     |
| array-typed options                                                     | [avutil.md](avutil.md) §4.15    |
| `avcodec_get_supported_config` for a codec's supported configurations   | [avcodec.md](avcodec.md) §4.3   |
| `AV_PROFILE_UNKNOWN` and `AV_LEVEL_UNKNOWN`                             | [avformat.md](avformat.md) §4.4 |
| coded side data on codec parameters                                     | [avformat.md](avformat.md) §4.4 |
| frame-based scaling (`sws_scale_frame`) and the scaler `threads` option | [swscale.md](swscale.md) §4.5   |

Rules for a conditional a later FFmpeg release makes necessary:

- **V5.** A conditional MUST compare a library version, and MUST name the
  version at which the newer facility appeared. Both sides MUST be reachable
  inside the supported range; a conditional with one reachable side is
  removed.
- **V6.** A feature test on a preprocessor name is allowed only for a name
  checked to be a macro in the headers of every supported release. An
  enumeration member is not a macro.
- **V7.** The stubs MUST build without deprecation warnings against the newest
  supported release. FFmpeg removes at each major release what the previous
  one deprecated.

## 3. Enumerations follow the headers

The constructors of every generated variant type are those the headers present
at build time declare ([build.md](build.md) §3). A program that names a
constructor absent from the installed headers fails to compile.

The bindings may be loaded against an FFmpeg library newer than the headers
they were built with. [binding-contract.md](binding-contract.md) §3 states
what a conversion does with a value the table lacks.

## 4. Reported versions

`Avutil.version`, `Avcodec.version`, `Av.avformat_version`,
`Swresample.version` and `Swscale.version` report the library loaded at run
time, not the headers the stubs were compiled against.

## 5. OCaml range

- **V8.** The minimum is **OCaml 5.3**.
- **V9.** The bindings MUST be correct in a program that runs several domains
  ([binding-contract.md](binding-contract.md) §6).
- **V10.** The conformance suite runs on the minimum and on the newest
  released OCaml ([tests.md](tests.md) §8).

What the specification needs from the runtime exists in every OCaml 5: one
threading model with domains, and a C interface that provides, under its
documented names, what the stubs need for options, byte sequences and thread
registration. Nothing in it needs a later release. 5.3 is the oldest version
these bindings are known to have been built and tested on.

## 6. Not verified

- FFmpeg 7.0 and earlier are outside the range and were never run.
- This specification was checked against no OCaml version other than 5.5.0.
- The floor release was built and run on Linux aarch64 only.
