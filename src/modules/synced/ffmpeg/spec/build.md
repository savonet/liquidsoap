# Build, detection and enumeration generation

The FFmpeg packages and libraries, what detection requires of each, the
enumeration tables derived from FFmpeg's headers, the installed C headers and
how the tests are wired into the build. Detection, gating, the install check,
warnings, the source of truth of generated code, variant constants and the
rules every installed header follows are the generic
[build.md](../../spec/build.md); §2 answers its §2 under the same numbers.

Advice specific to dune and the OCaml toolchain is in
[language-notes/build-system.md](../../spec/language-notes/build-system.md).

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

D1 to D15 are the [generic build.md §2](../../spec/build.md#2-availability-detection), for
each of the seven libraries of §1 in every context. FFmpeg's answers:

- **§2.1, D2.** The C libraries a library requires are those of its row in
  §1, and the binding libraries it depends on are those of the same row.
  `LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS` takes the library names of §1.
- **§2.2.** pkg-config is selected per context by the generic rules; nothing
  FFmpeg-specific.
- **§2.3, D6.** The minimum version of each C library is the one it has in
  FFmpeg 7.1, the floor of [compatibility.md](compatibility.md) V1, as listed
  in its §1.3.
- **§2.4, D7.** Applies unchanged.
- **§2.5, D11.** The build reports the availability of each of the seven
  libraries.
- **§2.7, D15.** Both ends of the supported FFmpeg range are built in
  continuous integration ([tests.md](tests.md) §8).

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

The member set is obtained as the
[generic build.md §3.1](../../spec/build.md#31-source-of-truth) states (G3), and the generator
fails as G7 states, naming the header and the enumeration.

- **G4.** For an enumeration, the members are the enumerators of the named C
  `enum` as they appear after preprocessing, in declaration order. Members
  guarded by a false conditional are absent.
- **G5.** For a macro family, the members are the macros with the given prefix
  that are defined after preprocessing. Macros guarded by a false conditional
  are absent.
- **G6.** The C value of a member is never read by the generator. The table
  refers to the C constant by name and the C compiler supplies its value.

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

| Table                   | Library      | Header                       | Source                               | C prefix                     |
| ----------------------- | ------------ | ---------------------------- | ------------------------------------ | ---------------------------- |
| `Pixel_format`          | `avutil`     | `libavutil/pixfmt.h`         | enum `AVPixelFormat`                 | `AV_PIX_FMT_`                |
| `Pixel_format_flag`     | `avutil`     | `libavutil/pixdesc.h`        | macros                               | `AV_PIX_FMT_FLAG_`           |
| `Color_space`           | `avutil`     | `libavutil/pixfmt.h`         | enum `AVColorSpace`                  | `AVCOL_SPC_`                 |
| `Color_range`           | `avutil`     | `libavutil/pixfmt.h`         | enum `AVColorRange`                  | `AVCOL_RANGE_`               |
| `Color_primaries`       | `avutil`     | `libavutil/pixfmt.h`         | enum `AVColorPrimaries`              | `AVCOL_PRI_`                 |
| `Color_trc`             | `avutil`     | `libavutil/pixfmt.h`         | enum `AVColorTransferCharacteristic` | `AVCOL_TRC_`                 |
| `Chroma_location`       | `avutil`     | `libavutil/pixfmt.h`         | enum `AVChromaLocation`              | `AVCHROMA_LOC_`              |
| `Sample_format`         | `avutil`     | `libavutil/samplefmt.h`      | enum `AVSampleFormat`                | `AV_SAMPLE_FMT_`             |
| `Channel_layout`        | `avutil`     | `libavutil/channel_layout.h` | macros                               | `AV_CH_LAYOUT_`              |
| `Hw_device_type`        | `avutil`     | `libavutil/hwcontext.h`      | enum `AVHWDeviceType`                | `AV_HWDEVICE_TYPE_`          |
| `Media_types`           | `avutil`     | `libavutil/avutil.h`         | enum `AVMediaType`                   | `AVMEDIA_TYPE_`              |
| `Subtitle_type`         | `avutil`     | `libavcodec/avcodec.h`       | enum `AVSubtitleType`                | `SUBTITLE_`                  |
| `Subtitle_flag`         | `avutil`     | `libavcodec/avcodec.h`       | macros                               | `AV_SUBTITLE_FLAG_`          |
| `Codec_capabilities`    | `avcodec`    | `libavcodec/codec.h`         | macros                               | `AV_CODEC_CAP_`              |
| `Codec_properties`      | `avcodec`    | `libavcodec/codec_desc.h`    | macros                               | `AV_CODEC_PROP_`             |
| `Hw_config_method`      | `avcodec`    | `libavcodec/codec.h`         | enumerators with the prefix          | `AV_CODEC_HW_CONFIG_METHOD_` |
| `Frame_side_data_type`  | `avutil`     | `libavutil/frame.h`          | enum `AVFrameSideDataType`           | `AV_FRAME_DATA_`             |
| `Side_data_prop`        | `avutil`     | `libavutil/frame.h`          | enum `AVSideDataProps`               | `AV_SIDE_DATA_PROP_`         |
| `Packet_side_data_type` | `avcodec`    | `libavcodec/packet.h`        | enum `AVPacketSideDataType`          | `AV_PKT_DATA_`               |
| `Codec_id`              | `avcodec`    | `libavcodec/codec_id.h`      | enum `AVCodecID`, five types (below) | `AV_CODEC_ID_`               |
| `Swresample_options`    | `swresample` | `libswresample/swresample.h` | three enums, three types (below)     | `SWR_`                       |

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
its constructors, named after the type, in declaration order. The generator
does not read C values (G6), so it cannot order a list by them.

### 3.6 Variant constants in C

The algorithm, G10 and G11 are the
[generic build.md §3.2](../../spec/build.md#32-variant-constants-in-c).

### 3.7 Conversions

[binding-contract.md](binding-contract.md) §3 and the generic contract own what a conversion does with
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

H1 and H3 are the [generic build.md §4](../../spec/build.md#4-installed-c-headers).

- **H2.** The declarations a header carries are an interface between binding
  libraries of the same version (P1). Each library file states in its §1 what
  it provides to its dependents.

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
