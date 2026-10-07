# Build, detection and enumeration generation

What decides which libraries are built, how the enumeration tables are derived
from FFmpeg's headers, and how the libraries, their C headers and their tests
are wired into the build.

Advice specific to dune and the OCaml toolchain is in
[language-notes/build-system.md](language-notes/build-system.md).

## 1. Packages and libraries

The project defines eight opam packages. Each of the first seven holds exactly
one OCaml library. The eighth is an umbrella. These names are frozen.

| opam package        | OCaml library | Top module   | C libraries its stubs call         | Binding libraries it depends on |
| ------------------- | ------------- | ------------ | ---------------------------------- | ------------------------------- |
| `ffmpeg-avutil`     | `avutil`      | `Avutil`     | libavutil, libavcodec              | none                            |
| `ffmpeg-avcodec`    | `avcodec`     | `Avcodec`    | libavcodec, libavutil              | `avutil`                        |
| `ffmpeg-av`         | `av`          | `Av`         | libavformat, libavcodec, libavutil | `avutil`, `avcodec`             |
| `ffmpeg-avfilter`   | `avfilter`    | `Avfilter`   | libavfilter, libavutil             | `avutil`                        |
| `ffmpeg-avdevice`   | `avdevice`    | `Avdevice`   | libavdevice, libavformat           | `av`                            |
| `ffmpeg-swresample` | `swresample`  | `Swresample` | libswresample, libavutil           | `avutil`, `avcodec`             |
| `ffmpeg-swscale`    | `swscale`     | `Swscale`    | libswscale, libavutil              | `avutil`                        |
| `ffmpeg`            | `ffmpeg`      | `Ffmpeg`     | none                               | all seven                       |

- **P1.** The opam dependencies MUST mirror this graph, each sibling pinned to
  the same version.
- **P2.** `avutil` depends on the OCaml threads library. `av` depends on the
  OCaml `unix` library, for `Unix.seek_command` in its interface.
- **P3.** `avutil` binds the subtitle structure of libavcodec
  ([avutil.md](avutil.md) §1), so it compiles and links against libavcodec.
- **P4.** The umbrella library has one module holding seven module aliases:
  `Avutil`, `Avcodec`, `Avfilter`, `Avdevice`, `Av`, `Swscale`, `Swresample`.
  It has no C code. It exists only when all seven libraries are available.
- **P5.** The stubs of a library MUST include headers of, and call functions
  of, only the C libraries listed in its row.

## 2. Availability detection

### 2.1 What detection decides

For each binding library and each build context, detection decides whether the
library is **available** and, when it is, the C flags its stubs compile with
and the link flags they link with.

- **D1.** Detection MUST be a build action that runs per library and per
  context. It MUST NOT fail the build when a library is unavailable.
- **D2.** A library is available when all of these hold:
  1. its name is not excluded (§2.2);
  2. pkg-config is found;
  3. pkg-config reports every C library of its row in §2.3 at the stated
     minimum version;
  4. every binding library it depends on (§1) is available.
- **D3.** The C flags of an available library are the flags pkg-config reports
  for its C libraries. Its link flags are the link flags pkg-config reports
  for them, after the filter of §2.4.
- **D4.** Flags MUST travel as argument vectors from pkg-config to the
  compiler. They MUST NOT be pasted into a shell command line.
- **D5.** Detection MUST be re-evaluated when the environment it reads
  changes, including an FFmpeg installed or upgraded under the same prefix
  since the previous build.

### 2.2 pkg-config selection and environment variables

Selection algorithm, given the context name `C` passed by the build system:

1. The effective context is the value of `LIQUIDSOAP_DUNE_TARGET` when that
   variable is set, otherwise `C`.
2. `<ctx>` is the effective context with every `.` replaced by `_`.
3. When `PKG_CONFIG_PATH_<ctx>` is set, its value is the pkg-config search
   path for this detection.
4. When `PKG_CONFIG_<ctx>` is set, its value is the pkg-config executable for
   this detection.
5. Otherwise the ambient `PKG_CONFIG_PATH` and `PKG_CONFIG` apply.

The substitutions affect only the detection being run.

| Variable                              | Effect                                                                        |
| ------------------------------------- | ----------------------------------------------------------------------------- |
| `LIQUIDSOAP_DUNE_TARGET`              | Replaces the context name in the selection above, in every context.           |
| `PKG_CONFIG_PATH_<ctx>`               | pkg-config search path for context `<ctx>`.                                   |
| `PKG_CONFIG_<ctx>`                    | pkg-config executable for context `<ctx>`.                                    |
| `PKG_CONFIG`, `PKG_CONFIG_PATH`       | Ambient pkg-config executable and search path.                                |
| `LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS`     | Space-separated list of library names of §1. A listed library is unavailable. |
| `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL` | When equal to `true`, disables the install-time check of §2.6.                |

These names and meanings are compatibility surface.
[cross-compilation.md](cross-compilation.md) §2 states what a cross build
relies on.

### 2.3 Per-library requirements

The minimum versions are those of the oldest supported FFmpeg release
([compatibility.md](compatibility.md) §1).

| Library      | pkg-config requirement                                                         |
| ------------ | ------------------------------------------------------------------------------ |
| `avutil`     | `libavutil >= 59.39.100`, `libavcodec >= 61.19.100`                            |
| `avcodec`    | `libavcodec >= 61.19.100`, `libavutil >= 59.39.100`                            |
| `av`         | `libavformat >= 61.7.100`, `libavcodec >= 61.19.100`, `libavutil >= 59.39.100` |
| `avfilter`   | `libavfilter >= 10.4.100`, `libavutil >= 59.39.100`                            |
| `avdevice`   | `libavdevice >= 61.3.100`, `libavformat >= 61.7.100`                           |
| `swscale`    | `libswscale >= 8.3.100`, `libavutil >= 59.39.100`                              |
| `swresample` | `libswresample >= 5.3.100`, `libavutil >= 59.39.100`                           |

- **D6.** Each minimum MUST be the version that library has in the oldest
  supported release, and the bindings MUST compile and pass the conformance
  suite against that release ([tests.md](tests.md) §8).

### 2.4 Windows link-flag filter

pkg-config for a static FFmpeg returns linker options written for a C link
line. Some are rejected or misread when they pass through the OCaml toolchain.

- **D7.** When the target's OS type is `Win32`, these link flags MUST be
  removed and every other flag kept in order:
  - every flag of three or more characters that starts with `-Wl`;
  - `-static-libgcc`;
  - `-lssp`;
  - `-lmingw32`.
- **D8.** For any other OS type the link flags are unchanged. The filter never
  applies to C flags.

### 2.5 Gating

- **D9.** A binding library is part of the build exactly when it is
  available. When unavailable, its stubs are not compiled and nothing can
  depend on it.
- **D10.** Availability is the only reason a library may be absent. With a
  library available, any failure to generate its tables, compile its stubs or
  resolve its dependencies MUST fail the build. No library may be declared
  optional to the build system on top of its availability gate.
- **D11.** The build MUST report, in its output, the availability of each of
  the seven libraries.

### 2.6 Install-time check

Every package may legally be empty, so that an unavailable library does not
break a workspace build.

- **D12.** The installation target of each binding package MUST fail, with a
  message naming the library, when that library is unavailable.
- **D13.** The check is disabled when `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL`
  equals `true`. A package whose library is unavailable then installs empty.

### 2.7 Compiler warnings

- **D14.** The stubs SHOULD be compiled with the compiler's general warning
  sets enabled.
- **D15.** A warning MUST NOT be an error in a packaged build. Warnings are
  promoted to errors in the development profile and in continuous integration
  only, where both ends of the supported FFmpeg range are built
  ([tests.md](tests.md) §8).

A warning promoted to an error is tested only on the versions someone builds;
a packaged build meets versions nobody built.

## 3. Enumeration tables

### 3.1 Principle

FFmpeg enumerations and flag families are OCaml polymorphic variant types. The
set of constructors of each type is derived at build time from the FFmpeg
headers the bindings are compiled against. One table per type pairs each
constructor with its C constant, and both conversion directions come from that
table.

- **G1.** The set of constructors of a generated type is the set of members
  the target's headers declare for it, after the exclusions of §3.3. A program
  that names a constructor absent from those headers fails to compile. This is
  part of the interface.
- **G2.** The derivation of constructor names (§3.4) is compatibility surface:
  a member keeps its constructor name across versions of the bindings.

### 3.2 Source of truth

- **G3.** The member set MUST be obtained by running the target's C compiler
  as a preprocessor on a translation unit that includes the header by its
  public include path, with the library's detected C flags. The compiler finds
  the header exactly as it will when compiling the stubs.
- **G4.** For an enumeration, the members are the enumerators of the named C
  `enum` as they appear after preprocessing, in declaration order. Members
  guarded by a false conditional are absent.
- **G5.** For a macro family, the members are the macros with the given prefix
  that are defined after preprocessing. Macros guarded by a false conditional
  are absent.
- **G6.** The C value of a member is never read by the generator. The table
  refers to the C constant by name and the C compiler supplies its value.
- **G7.** The generator MUST exit with a failure, naming the header and the
  enumeration, when: the preprocessor cannot be run or fails; the header is
  not found; the enumeration is not found; a table has no member. It MUST NOT
  fall back to another way of reading the header.

### 3.3 Exclusions

Markers that FFmpeg declares inside an enumeration without their being a value
of it are not constructors:

| Enumeration             | Excluded members                                                                     |
| ----------------------- | ------------------------------------------------------------------------------------ |
| any                     | a member whose name ends in `_NB`                                                    |
| codec identifiers       | `AV_CODEC_ID_FIRST_AUDIO`, `AV_CODEC_ID_FIRST_SUBTITLE`, `AV_CODEC_ID_FIRST_UNKNOWN` |
| colour primaries        | `AVCOL_PRI_EXT_BASE`                                                                 |
| transfer characteristic | `AVCOL_TRC_EXT_BASE`                                                                 |
| dither type             | `SWR_DITHER_NS`                                                                      |

- **G8.** A member declared after a marker in the same enumeration is a
  member. No marker ends a table.
- **G9.** No other member is excluded, renamed or guarded. Alias members whose
  C value equals another member's are constructors.

### 3.4 Constructor names

Given a member name with the table's C prefix removed, `M`:

1. When the first character of `M` is a digit, prepend `_`.
2. Upper-case the first character (an underscore is unchanged) and lower-case
   all the others.
3. Compute the variant constant (§3.6) of the result. When that constant was
   already produced earlier in the same type, append `_` to the name and go
   back to step 1.

Examples: `AV_PIX_FMT_YUV420P` gives `` `Yuv420p ``; `AV_PIX_FMT_0RGB` gives
`` `_0rgb ``; `AV_CH_LAYOUT_5POINT1_BACK` gives `` `_5point1_back ``;
`AVCOL_SPC_BT2020_NCL` gives `` `Bt2020_ncl ``; `SWR_DITHER_RECTANGULAR`
(prefix `SWR_`) gives `` `Dither_rectangular ``.

### 3.5 The tables

"enum" rows take the enumerators of the C enumeration; "macros" rows take the
macro family. The OCaml type is named `t` in a module named after the table
unless stated.

| Table                | Library      | Header                       | Source                               | C prefix                     |
| -------------------- | ------------ | ---------------------------- | ------------------------------------ | ---------------------------- |
| `Pixel_format`       | `avutil`     | `libavutil/pixfmt.h`         | enum `AVPixelFormat`                 | `AV_PIX_FMT_`                |
| `Pixel_format_flag`  | `avutil`     | `libavutil/pixdesc.h`        | macros                               | `AV_PIX_FMT_FLAG_`           |
| `Color_space`        | `avutil`     | `libavutil/pixfmt.h`         | enum `AVColorSpace`                  | `AVCOL_SPC_`                 |
| `Color_range`        | `avutil`     | `libavutil/pixfmt.h`         | enum `AVColorRange`                  | `AVCOL_RANGE_`               |
| `Color_primaries`    | `avutil`     | `libavutil/pixfmt.h`         | enum `AVColorPrimaries`              | `AVCOL_PRI_`                 |
| `Color_trc`          | `avutil`     | `libavutil/pixfmt.h`         | enum `AVColorTransferCharacteristic` | `AVCOL_TRC_`                 |
| `Chroma_location`    | `avutil`     | `libavutil/pixfmt.h`         | enum `AVChromaLocation`              | `AVCHROMA_LOC_`              |
| `Sample_format`      | `avutil`     | `libavutil/samplefmt.h`      | enum `AVSampleFormat`                | `AV_SAMPLE_FMT_`             |
| `Channel_layout`     | `avutil`     | `libavutil/channel_layout.h` | macros                               | `AV_CH_LAYOUT_`              |
| `Hw_device_type`     | `avutil`     | `libavutil/hwcontext.h`      | enum `AVHWDeviceType`                | `AV_HWDEVICE_TYPE_`          |
| `Media_types`        | `avutil`     | `libavutil/avutil.h`         | enum `AVMediaType`                   | `AVMEDIA_TYPE_`              |
| `Subtitle_type`      | `avutil`     | `libavcodec/avcodec.h`       | enum `AVSubtitleType`                | `SUBTITLE_`                  |
| `Subtitle_flag`      | `avutil`     | `libavcodec/avcodec.h`       | macros                               | `AV_SUBTITLE_FLAG_`          |
| `Codec_capabilities` | `avcodec`    | `libavcodec/codec.h`         | macros                               | `AV_CODEC_CAP_`              |
| `Codec_properties`   | `avcodec`    | `libavcodec/codec_desc.h`    | macros                               | `AV_CODEC_PROP_`             |
| `Hw_config_method`   | `avcodec`    | `libavcodec/codec.h`         | enumerators with the prefix          | `AV_CODEC_HW_CONFIG_METHOD_` |
| `Codec_id`           | `avcodec`    | `libavcodec/codec_id.h`      | enum `AVCodecID`, five types (below) | `AV_CODEC_ID_`               |
| `Swresample_options` | `swresample` | `libswresample/swresample.h` | three enums, three types (below)     | `SWR_`                       |

The first member of an enumeration is a member like any other: `` `None `` is
a constructor of `Pixel_format.t`, `Sample_format.t`, `Hw_device_type.t` and
`Subtitle_type.t`, and `` `Unknown `` of `Media_types.t`.

**`Codec_id`.** `AVCodecID` is one C enumeration that FFmpeg divides by
position with three range markers. It gives five OCaml types:

| OCaml type | Members                                                                                  |
| ---------- | ---------------------------------------------------------------------------------------- |
| `video`    | `WRAPPED_AVFRAME`, `NONE`, then every member declared before `FIRST_AUDIO`, after `NONE` |
| `audio`    | `WRAPPED_AVFRAME`, `NONE`, then every member between `FIRST_AUDIO` and `FIRST_SUBTITLE`  |
| `subtitle` | `NONE`, then every member between `FIRST_SUBTITLE` and `FIRST_UNKNOWN`                   |
| `unknown`  | `NONE`, then every member declared after `FIRST_UNKNOWN`                                 |
| `codec_id` | every member of the enumeration                                                          |

The exclusions of §3.3 apply to each: the three markers are in no type.

The position of an identifier in the enumeration decides its family. FFmpeg
types a few codecs as audio or video while declaring their identifier after
`FIRST_UNKNOWN`; [avcodec.md](avcodec.md) §4 states what the per-family
operations do with them.

**`Swresample_options`.**

| OCaml type    | C enumeration   | Example constructors                                |
| ------------- | --------------- | --------------------------------------------------- |
| `dither_type` | `SwrDitherType` | `` `Dither_none ``, `` `Dither_ns_shibata ``        |
| `engine`      | `SwrEngine`     | `` `Engine_swr ``, `` `Engine_soxr ``               |
| `filter_type` | `SwrFilterType` | `` `Filter_type_cubic ``, `` `Filter_type_kaiser `` |

**Value lists.** For each type the generated module also defines a list of all
its constructors, named after the type. An enumeration's list is in
declaration order. A macro family's list is in ascending order of C value.

### 3.6 Variant constants in C

An OCaml constant polymorphic variant is an immediate integer. For a
constructor name `s`, without the backquote:

1. `h = 0`; for each byte `c` of `s`, `h = 223 * h + c`, in unsigned
   arithmetic.
2. Keep the low 31 bits of `h`.
3. The constant is `2 * h + 1`, taken as a signed 32-bit integer and
   sign-extended to the platform word.

The number is directly comparable to an OCaml value received by a stub and can
be returned as an OCaml value without further encoding.

Known answers: `Jpeg` = 1652440465, `Unspecified` = -789514449, `Audio` =
1968951661, `Video` = -1806497609, `None` = 1741061553, `_0rgb` =
-1505465991.

- **G10.** A variant constant used by hand-written stub code MUST come from
  this algorithm, through generated definitions. A constant typed by hand is
  not allowed.
- **G11.** A variant constant MUST be held in C in a type wide enough for a
  negative OCaml immediate. It MUST NOT pass through a C enumeration type.

### 3.7 Conversions

[binding-contract.md](binding-contract.md) §3 owns what a conversion does with
a value that has no entry and which constructor an aliased C value converts
to.

## 4. C interface between the libraries

`avutil`, `avcodec` and `av` each install C headers that the stubs of
dependent libraries include. The file names are frozen:

| Package          | Installed headers                                                             |
| ---------------- | ----------------------------------------------------------------------------- |
| `ffmpeg-avutil`  | `avutil_stubs.h`, `polymorphic_variant_values_stubs.h`, `media_types_stubs.h` |
| `ffmpeg-avcodec` | `avcodec_stubs.h`                                                             |
| `ffmpeg-av`      | `av_stubs.h`                                                                  |

- **H1.** Every installed header MUST be includable by any number of
  translation units of one program: it has an include guard and defines no
  object or function with external linkage.
- **H2.** The declarations a header carries are an interface between binding
  libraries of the same version (P1). Each library file states in its §1 what
  it provides to its dependents.
- **H3.** A function declared in an installed header MUST state, in the
  header, whether it requires the OCaml runtime lock and whether it can raise.

## 5. Tests in the build

- **T1.** The conformance suite ([tests.md](tests.md)) is attached to the
  build alias `ffmpeg_citest`. The name is frozen.
- **T2.** The alias MUST exist whatever the availability of the libraries.
  Requesting it with a library unavailable MUST fail with a message naming
  the unavailable libraries.
- **T3.** The suite is not attached to the build system's standard test
  alias: it needs an FFmpeg build with the components of
  [tests.md](tests.md) §8.
- **T4.** The example programs are outside this specification. A build MAY
  compile them.

## 6. Packaging

- **K1.** Each binding package depends, at build time only, on pkg-config and
  on FFmpeg's development files.
- **K2.** A failed build of a binding package SHOULD point the user at the
  minimum FFmpeg version.
- **K3.** The seven binding packages and the umbrella are released together
  under one version.
