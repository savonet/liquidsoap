# Build, detection and code generation

As-built description of everything that decides what gets built and of the
code generated at build time. Examples marked _observed_ are copied from a
build against FFmpeg git `N-126774-g6242bd002d` (libavutil 61.7.100,
libavcodec 63.14.100, libavformat 63.7.100, libavfilter 12.4.100,
libavdevice 63.2.100, libswresample 7.3.100, libswscale 10.2.100) with
OCaml 5.5.0 and gcc on Linux aarch64. Anything else is marked _derived from
the source, not observed_.

Mechanisms specific to dune and the OCaml toolchain are in
[language-notes/build.md](language-notes/build.md).

## 1. Packages and library graph

The project is a dune project named `ffmpeg`, version `1.4.0`, licence
LGPL-2.1-only, requiring dune 3.23 and OCaml >= 4.12. It defines eight opam
packages. Each of the first seven holds exactly one OCaml library with one
C stub file; the eighth is an umbrella.

| opam package        | OCaml library name | top module   | C library bound                    | OCaml library dependencies                |
| ------------------- | ------------------ | ------------ | ---------------------------------- | ----------------------------------------- |
| `ffmpeg-avutil`     | `avutil`           | `Avutil`     | libavutil (and libavcodec headers) | `threads`                                 |
| `ffmpeg-avcodec`    | `avcodec`          | `Avcodec`    | libavcodec                         | `ffmpeg-avutil`                           |
| `ffmpeg-av`         | `av`               | `Av`         | libavformat                        | `ffmpeg-avutil`, `ffmpeg-avcodec`, `unix` |
| `ffmpeg-avfilter`   | `avfilter`         | `Avfilter`   | libavfilter                        | `ffmpeg-avutil`                           |
| `ffmpeg-avdevice`   | `avdevice`         | `Avdevice`   | libavdevice                        | `ffmpeg-av`                               |
| `ffmpeg-swresample` | `swresample`       | `Swresample` | libswresample                      | `ffmpeg-avutil`, `ffmpeg-avcodec`         |
| `ffmpeg-swscale`    | `swscale`          | `Swscale`    | libswscale                         | `ffmpeg-avutil`                           |
| `ffmpeg`            | `ffmpeg`           | `Ffmpeg`     | none                               | all seven above                           |

Dependency graph (an arrow points to a dependency):

```
avdevice -> av -> avcodec -> avutil
                  av -> avutil
swresample -> avcodec, avutil
avfilter -> avutil
swscale  -> avutil
ffmpeg   -> all seven
```

The opam dependencies mirror this graph, each sibling pinned to the same
version (`= version`). Every binding package also depends, at build time
only, on `conf-pkg-config`, `conf-ffmpeg` and `dune-configurator`.
`ffmpeg-avutil` additionally depends on `base-threads`. Every binding
package conflicts with `ffmpeg < 0.5.0`. Every package is declared as
allowed to be empty.

The umbrella library `ffmpeg` has one module, `Ffmpeg`, that contains seven
module aliases and nothing else: `Avutil`, `Avcodec`, `Avfilter`,
`Avdevice`, `Av`, `Swscale`, `Swresample`. It has no C code. It is the only
library marked optional: it is silently skipped when one of its
dependencies is absent.

Each library's modules are not wrapped under a prefix beyond dune's
default: the generated enum modules (section 3) are ordinary modules of the
library they are generated into, reachable as `Avutil.Pixel_format` and so
on only through the re-exports the hand-written module makes.

## 2. Availability detection

Each of the seven binding libraries is built only when pkg-config finds its
C library at a sufficient version. Detection runs once per library and per
build context, as a build action, and writes four files that the rest of
the build reads.

### 2.1 The detection program

Command line:

```
detect [--os-type <type>] [--context <context>] <name> <package> <expr> [extra-cflags...]
```

`--os-type`, when present, must be the first argument; `--context`, when
present, must come next. With fewer than three positional arguments the
program prints the usage line above on standard error and exits 1. In every
other case it exits 0, whether or not the library was found.

Algorithm:

1. `os_type` is the `--os-type` value, or the OCaml `Sys.os_type` of the
   detection program when the flag is absent.
2. Select the pkg-config environment for the build context:
   - with `--context C`: the context is the value of the environment
     variable `LIQUIDSOAP_DUNE_TARGET` when set, otherwise `C`;
   - without `--context`: the context is `LIQUIDSOAP_DUNE_TARGET` when set,
     otherwise no context handling happens.
     For a context, compute its sanitised name by replacing every `.` with
     `_`. If `PKG_CONFIG_PATH_<sanitised>` is set, copy its value into
     `PKG_CONFIG_PATH`. If `PKG_CONFIG_<sanitised>` is set, copy its value
     into `PKG_CONFIG`. Both copies affect only this process and its
     children.
3. If `LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS` is set and, split on single spaces,
   contains `<name>` as one element, the library is unavailable with empty
   flags. pkg-config is not run.
4. Otherwise locate pkg-config through dune-configurator. When it is not
   found, the library is unavailable with empty flags.
5. Otherwise ask pkg-config whether `<expr>` is satisfied. When it is not,
   the library is unavailable with empty flags. The pkg-config error text
   is discarded.
6. Otherwise the library is available. Its C flags are
   `pkg-config --cflags <package>` split on blanks, followed by the
   `extra-cflags` arguments in order. Its link flags are
   `pkg-config --libs <package>` split on blanks.
7. Apply the Windows filter (2.4) to the link flags.
8. Write the four output files (2.5) into the current directory.

### 2.2 Environment variables

| Variable                              | Read by                                                              | Effect                                                                                                                                                                          |
| ------------------------------------- | -------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `LIQUIDSOAP_DUNE_TARGET`              | detection, include-path discovery                                    | Overrides the dune context name used to pick the two per-context variables below. Also enables context handling when the program is run without a context argument.             |
| `PKG_CONFIG_PATH_<ctx>`               | detection, include-path discovery                                    | Copied into `PKG_CONFIG_PATH` before pkg-config runs. `<ctx>` is the context name with `.` replaced by `_` (context `default.windows` reads `PKG_CONFIG_PATH_default_windows`). |
| `PKG_CONFIG_<ctx>`                    | detection, include-path discovery                                    | Copied into `PKG_CONFIG`, the pkg-config executable to run.                                                                                                                     |
| `PKG_CONFIG`                          | include-path discovery directly; detection through dune-configurator | The pkg-config executable.                                                                                                                                                      |
| `PKG_CONFIG_PATH`                     | pkg-config itself                                                    | Search path, possibly overwritten as above.                                                                                                                                     |
| `LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS`     | detection only                                                       | Space-separated list of detection names (`avutil`, `avcodec`, `av`, `avfilter`, `avdevice`, `swscale`, `swresample`). A listed library is reported unavailable.                 |
| `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL` | the build description                                                | When equal to `true`, the install-time check (2.7) is disabled. Default `false`.                                                                                                |
| `GITHUB_ACTIONS`                      | the test runner                                                      | Changes its log framing only (section 5).                                                                                                                                       |

### 2.3 Per-library queries

Every library passes the same four extra C flags:
`-Wall -Wextra -Werror=unused-variable -Werror=unused-parameter`.
Every invocation passes `--os-type` with the build system's target OS type
and `--context` with the dune context name.

| `<name>`     | `<package>` (flags taken from) | `<expr>` (existence and minimum versions)          |
| ------------ | ------------------------------ | -------------------------------------------------- |
| `avutil`     | `libavutil libavcodec`         | `libavutil >= 57.24.100, libavcodec >= 59.24.100`  |
| `avcodec`    | `libavcodec`                   | `libavcodec >= 59.24.100`                          |
| `av`         | `libavutil libavformat`        | `libavutil >= 57.24.100, libavformat >= 59.19.100` |
| `avfilter`   | `libavfilter`                  | `libavfilter >= 8.28.100`                          |
| `avdevice`   | `libavdevice`                  | `libavdevice >= 59.5.100`                          |
| `swscale`    | `libswscale`                   | `libswscale >= 6.5.100`                            |
| `swresample` | `libswresample`                | `libswresample >= 4.5.100`                         |

`avutil` needs libavcodec because two of its enum tables (subtitle type and
subtitle flag) are read from a libavcodec header and its stubs use those
constants.

Each stub file is linked only with the link flags of its own row. _Observed_
link flags: `avutil` gets `-L<prefix>/lib -lavcodec -lavutil`, `av` gets
`-L<prefix>/lib -lavformat -lavutil`, the others get `-L<prefix>/lib` and
the single `-l<lib>`.

The detection rule depends on the whole environment ("universe"): it is
re-run on every build invocation.

### 2.4 Windows link-flag filter

When `os_type` is exactly `Win32`, these link flags are removed; everything
else is kept in order:

- every flag of three or more characters that starts with `-Wl`;
- `-static-libgcc`;
- `-lssp`;
- `-lmingw32`.

For any other `os_type` the link flags are unchanged. The filter is never
applied to C flags.

### 2.5 Output files

All four are written to the detection directory. `<name>` is the first
positional argument.

| File                          | Content                                                                                                                |
| ----------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `<name>_available`            | The text `true` or `false`. No newline.                                                                                |
| `<name>_c_flags.sexp`         | `(`, the C flags joined by single spaces, `)`. No newline, no quoting or escaping of the flags. `()` when unavailable. |
| `<name>_c_flags`              | One C flag per line, each line ended by a newline. Empty file when unavailable.                                        |
| `<name>_c_library_flags.sexp` | Same format as the C flags s-expression, for the filtered link flags.                                                  |

_Observed_ (`avutil`):

```
avutil_available:            true
avutil_c_flags.sexp:         (-I<prefix>/include -Wall -Wextra -Werror=unused-variable -Werror=unused-parameter)
avutil_c_flags:              -I<prefix>/include
                             -Wall
                             -Wextra
                             -Werror=unused-variable
                             -Werror=unused-parameter
avutil_c_library_flags.sexp: (-L<prefix>/lib -lavcodec -lavutil)
```

The s-expression files feed the C compile and link flags of the library.
The line-per-flag file feeds the enum generator's command line (section 3).

### 2.6 Gating a library

Each binding library is enabled exactly when the content of its
`<name>_available` file equals the string `true`. When disabled, the
library does not exist for the build: its stubs are not compiled and
nothing can depend on it.

Availability of one library is computed independently of the others. The
dependency graph of section 1 is not consulted by detection.

The build system's standard C flag set for stub files is replaced, not
extended, by the content of `<name>_c_flags.sexp`. The compiler driver still
adds the C flags of the OCaml configuration itself (optimisation,
position-independent code, threads). The link flags of each library are exactly the content
of `<name>_c_library_flags.sexp`.

### 2.7 Install-time check

Each binding package carries a rule attached to its installation target.
The rule feeds the library's `<name>_available` file on standard input to a
check program called with `<name>` as its only argument. The check program
reads one line, trims it, and when it differs from `true` prints

```
Error: <name> C library not found via pkg-config.
```

on standard error and exits 1, which fails the installation build of that
package. This is what turns "library disabled" into "package build failed"
for a packaged install, since every package may otherwise legally be empty.

The rule is disabled when `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL` equals
`true`. With the check disabled, a package whose C library is missing
installs as an empty package.

### 2.8 Top-level summary

A top-level rule produces a file `ffmpeg.config`, copied back into the
source directory until the next clean. Its action prints these lines on the
build's standard output, each value being the content of the corresponding
availability file:

```
ffmpeg library availability:
  avutil:     <true|false>
  avcodec:    <true|false>
  avfilter:   <true|false>
  avdevice:   <true|false>
  av:         <true|false>
  swscale:    <true|false>
  swresample: <true|false>
```

and then writes the empty string to `ffmpeg.config`. The file itself is
empty; the summary exists only in the build output. (_Derived from the
source, not observed_: this target was not built because it writes into the
source tree.)

### 2.9 The all-available gate

A second helper takes file paths as arguments, reads the first line of
each, and prints `true` when every one equals `true`, otherwise `false`,
with no newline. It is run over the seven availability files, in the order
`avutil`, `avcodec`, `avfilter`, `av`, `swscale`, `swresample`, `avdevice`,
and its output is stored as `all_libs_available` in the detection
directory. This single value gates tests and examples (section 5).

## 3. Enum table generation

FFmpeg enumerations and flag families are exposed to OCaml as polymorphic
variant types. The mapping between an OCaml constructor and a C constant is
a table generated at build time by scanning the installed FFmpeg headers as
text. One generator program produces every table.

### 3.1 Command line

```
gen_code <cc> <table-name> <mode> [c-flags...]
```

- `<cc>`: the C compiler command, as one argument (it may contain spaces
  and flags; it is pasted into a shell command).
- `<table-name>`: one of the eighteen names of 3.5, or
  `polymorphic_variant` (3.8). An unknown name aborts with the failure
  `gen_code: unknown generator <name>`.
- `<mode>`: `h` writes `<table-name>_stubs.h`; `ml` writes
  `<table-name>.ml`. Both go to the current directory. For
  `polymorphic_variant` only `h` is accepted and the file is
  `polymorphic_variant_values_stubs.h`. Any other mode aborts on an
  assertion.
- `[c-flags...]`: every argument from the fourth on. In the build these are
  the lines of the consuming library's `<name>_c_flags` file.

The two modes are two independent runs; each locates, reads and scans the
header again. One run produces one file.

### 3.2 Locating a header

Include-path discovery runs once per build context, before the generator is
compiled, and bakes a list of directories into the generator. Under
cross-compilation the generator that runs is the build machine's, so only the
`default` context's discovery takes effect
([cross-compilation.md](cross-compilation.md) §1.2).

Discovery algorithm (program argument: the dune context name):

1. Apply the context handling of 2.1 step 2 (same variables, same rule).
2. The pkg-config executable is `$PKG_CONFIG` when set, otherwise
   `pkg-config` found in `PATH`. When neither exists the result is the
   default list `/usr/local/include`, `/usr/include`.
3. Otherwise, for each of `libavutil`, `libavformat`, `libavfilter`,
   `libavcodec`, `libavdevice`, `libswresample`, `libswscale`:
   1. Run `pkg-config --cflags-only-I <package>`. On a non-zero exit, add
      the two default directories.
   2. Otherwise split the output on single spaces. Split each token on the
      character `I`. A token that yields exactly two pieces contributes its
      second piece, trimmed and cut at the first newline, as a directory.
      Every other token is ignored.
   3. When no directory resulted, run
      `pkg-config --variable=includedir <package>`; a non-empty first line
      is the directory. Otherwise add the two default directories.
4. Sort the collected directories and remove duplicates.
5. Emit them as an OCaml string list.

_Observed_ result: the single directory `<prefix>/include`.

Discovery does not consult `LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS` and always
queries all seven packages.

At run time the generator builds its search list this way: start from the
baked-in list; scan the `c-flags` arguments left to right; each argument
longer than two characters that starts with `-I` contributes the rest of
the argument as a directory, placed in front of the list unless already
present. The resulting order is: last `-I` flag first, first `-I` flag
next-to-last, baked-in directories last.

Each table names one or two header paths relative to an include directory
(for example `/libavcodec/codec_id.h`, then `/libavcodec/avcodec.h`). The
generator tries the first header name against every directory in search
order, then the second name against every directory, and takes the first
path that exists as a file. The path is the plain concatenation of
directory and header name.

When no candidate exists the generator prints

```
WARNING : None of the header files ["<h1>"; "<h2>"] where found
```

on standard error, leaves the output file empty, and exits 0.

### 3.3 Reading the header

Each table states whether it is preprocessed.

- Not preprocessed: the header file is read as lines.
- Preprocessed: the generator runs, through the system shell,
  `<cc> -E <c-flags joined by single spaces> <shell-quoted header path>`
  and reads its standard output as lines. The C flags are not quoted. The
  input therefore contains the header and everything it includes, with
  comments removed, macros expanded, conditional sections resolved, and
  line markers added. When the command cannot be started, or exits with a
  non-zero status, the generator discards what it read and reads the header
  file as plain lines instead. It prints nothing itself and exits 0; the
  shell's or compiler's own standard error still passes through.

Preprocessing is what resolves members guarded by version macros. Tables
that are not preprocessed see every `#define`, whatever conditional
section it sits in.

### 3.4 Scanning one block

A table file contains one or more blocks. A block is described by: a start
pattern, a member pattern, a stop pattern, a C prefix, a C type, a function
radix, an OCaml type name (default `t`), and a list of extra members
(default empty). All patterns are matched anchored at the first character
of a line; nothing anchors the end.

Member patterns come in two forms:

| Form  | Member pattern                                                                                                 | Used for                                                         |
| ----- | -------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------- |
| enum  | zero or more spaces or tabs, the C prefix, then one or more of `A`–`Z`, `0`–`9`, `_` (captured)                | C `enum` members                                                 |
| flags | `#define`, one space, the C prefix, then one or more of `A`–`Z`, `0`–`9`, `_` (captured), starting in column 0 | families of `#define` constants; never has start, stop or extras |

Scanning:

1. When the start pattern is non-empty, skip lines up to and including the
   first line that matches it. When no line matches, the block produces no
   output at all (no type, no table, no functions) and no diagnostic. The
   start line itself is never examined for a member.
2. Emit the extra members first, in the listed order, as if they had been
   scanned.
3. For each following line, in order:
   - when the stop pattern is non-empty and matches, stop; the stop line
     contributes nothing;
   - otherwise, when the member pattern matches, the captured text is a
     member;
   - otherwise skip the line.
     With an empty stop pattern scanning runs to the end of the input.
4. At most one member is taken per line. Text after the captured name
   (` = value`, `,`, a comment) is ignored. The C value is never read: the
   table refers to the C constant by name and lets the C compiler supply
   it.

Constructor name derivation, applied to each captured member name `M`:

1. When the first character of `M` is a digit, prepend `_`.
2. Upper-case the first character (an underscore is unchanged) and
   lower-case all the others.
3. Compute the variant hash (3.7) of the result. When that hash value was
   already produced earlier in the same block, append `_` to the name and
   go back to step 1. This rule covers both a true hash collision and the
   same member appearing twice.

There are no renames, no exclusion list and no per-entry version guards.
Every member that the scan yields is emitted, including alias members whose
C value equals another member's, and marker members such as
`AV_CODEC_ID_FIRST_AUDIO`.

_Observed_ names: `AV_PIX_FMT_YUV420P` gives `` `Yuv420p ``;
`AV_PIX_FMT_0RGB` gives `` `_0rgb ``; `AV_CH_LAYOUT_5POINT1_BACK` gives
`` `_5point1_back ``; `AVCOL_SPC_BT2020_NCL` gives `` `Bt2020_ncl ``;
`SWR_DITHER_RECTANGULAR` (prefix `SWR_`) gives `` `Dither_rectangular ``.
No trailing-underscore name occurs in the observed output.

### 3.5 The tables

`pp` = preprocessed. Patterns are written as the generator has them
(`[ \t]*` is "optional spaces or tabs"). "Members" is the count _observed_
with the FFmpeg version named at the top.

Tables with one block and OCaml type name `t`:

| Table name           | Header candidates, in order                       | pp  | Start pattern                        | Stop pattern                                           | C prefix                     | C type                               | Radix               | Members |
| -------------------- | ------------------------------------------------- | --- | ------------------------------------ | ------------------------------------------------------ | ---------------------------- | ------------------------------------ | ------------------- | ------- |
| `pixel_format`       | `libavutil/pixfmt.h`                              | yes | `enum AVPixelFormat`                 | `[ \t]*AV_PIX_FMT_NB`                                  | `AV_PIX_FMT_`                | `enum AVPixelFormat`                 | `PixelFormat`       | 272     |
| `color_space`        | `libavutil/pixfmt.h`                              | yes | `enum AVColorSpace`                  | `[ \t]*AVCOL_SPC_NB`                                   | `AVCOL_SPC_`                 | `enum AVColorSpace`                  | `ColorSpace`        | 19      |
| `color_range`        | `libavutil/pixfmt.h`                              | yes | `enum AVColorRange`                  | `[ \t]*AVCOL_RANGE_NB`                                 | `AVCOL_RANGE_`               | `enum AVColorRange`                  | `ColorRange`        | 3       |
| `color_primaries`    | `libavutil/pixfmt.h`                              | yes | `enum AVColorPrimaries`              | `[ \t]*AVCOL_PRI_NB`                                   | `AVCOL_PRI_`                 | `enum AVColorPrimaries`              | `ColorPrimaries`    | 16      |
| `color_trc`          | `libavutil/pixfmt.h`                              | yes | `enum AVColorTransferCharacteristic` | `[ \t]*AVCOL_TRC_NB`                                   | `AVCOL_TRC_`                 | `enum AVColorTransferCharacteristic` | `ColorTrc`          | 21      |
| `chroma_location`    | `libavutil/pixfmt.h`                              | yes | `enum AVChromaLocation`              | `[ \t]*AVCHROMA_LOC_NB`                                | `AVCHROMA_LOC_`              | `enum AVChromaLocation`              | `ChromaLocation`    | 7       |
| `hw_device_type`     | `libavutil/hwcontext.h`                           | yes | `enum AVHWDeviceType`                | `[ \t]*AV_HWDEVICE_TYPE_NONE ` (with a trailing space) | `AV_HWDEVICE_TYPE_`          | `enum AVHWDeviceType`                | `HwDeviceType`      | 15      |
| `sample_format`      | `libavutil/samplefmt.h`                           | yes | `enum AVSampleFormat`                | `[ \t]*AV_SAMPLE_FMT_NB`                               | `AV_SAMPLE_FMT_`             | `enum AVSampleFormat`                | `SampleFormat`      | 14      |
| `subtitle_type`      | `libavcodec/avcodec.h`                            | yes | `enum AVSubtitleType`                | a line starting with `};`                              | `SUBTITLE_`                  | `enum AVSubtitleType`                | `SubtitleType`      | 4       |
| `media_types`        | `libavutil/avutil.h` (listed twice)               | no  | `enum AVMediaType`                   | `[ \t]*AVMEDIA_TYPE_NB`                                | `AVMEDIA_TYPE_`              | `uint64_t`                           | `MediaTypes`        | 6       |
| `hw_config_method`   | `libavcodec/avcodec.h`                            | yes | none                                 | none                                                   | `AV_CODEC_HW_CONFIG_METHOD_` | `uint64_t`                           | `HwConfigMethod`    | 4       |
| `pixel_format_flag`  | `libavutil/pixdesc.h`                             | no  | flags form                           | —                                                      | `AV_PIX_FMT_FLAG_`           | `uint64_t`                           | `PixelFormatFlag`   | 10      |
| `channel_layout`     | `libavutil/channel_layout.h`                      | no  | flags form                           | —                                                      | `AV_CH_LAYOUT_`              | `uint64_t`                           | `ChannelLayout`     | 44      |
| `codec_capabilities` | `libavcodec/codec.h`, `libavcodec/avcodec.h`      | no  | flags form                           | —                                                      | `AV_CODEC_CAP_`              | `uint64_t`                           | `CodecCapabilities` | 18      |
| `codec_properties`   | `libavcodec/codec_desc.h`, `libavcodec/avcodec.h` | no  | flags form                           | —                                                      | `AV_CODEC_PROP_`             | `uint64_t`                           | `CodecProperties`   | 8       |
| `subtitle_flag`      | `libavcodec/avcodec.h`                            | no  | flags form                           | —                                                      | `AV_SUBTITLE_FLAG_`          | `int`                                | `SubtitleFlag`      | 1       |

Remarks on single-block tables, all _observed_:

- A table whose start pattern is the `enum …` line includes the first
  member of the enum (`None` in `pixel_format`, `sample_format`,
  `hw_device_type`, `subtitle_type`; `Unknown` in `media_types`).
- `hw_device_type`: its stop pattern matches no line of the preprocessed
  input, so the scan runs to the end of the input. No later line matches
  the member pattern, so the table holds exactly the enum's members.
- `color_primaries` and `color_trc` stop at the `_NB` member. Members
  declared after it in the same enum (`AVCOL_PRI_EXT_BASE`,
  `AVCOL_PRI_V_GAMUT`, `AVCOL_TRC_EXT_BASE`, `AVCOL_TRC_V_LOG`) are not in
  the table.
- `hw_config_method` has no delimiters: every line of the preprocessed
  `avcodec.h` translation unit that starts with the prefix is a member.
- `channel_layout` contains the alias defines (`_5point1point4_back`,
  `_7point1point4_back`, `_9point1point4_back`, `_7point1_top_back`), whose
  C values equal those of earlier members.
- `color_primaries` and `color_trc` contain alias members
  (`Smptest428_1`, `Smptest2084`) after the member they alias.

Table `codec_id`: headers `libavcodec/codec_id.h` then
`libavcodec/avcodec.h`, preprocessed, five blocks, all with C prefix
`AV_CODEC_ID_` and C type `enum AVCodecID`:

| OCaml type | Radix             | Start pattern                      | Stop pattern                       | Extras, in order          | Members |
| ---------- | ----------------- | ---------------------------------- | ---------------------------------- | ------------------------- | ------- |
| `video`    | `VideoCodecID`    | `[ \t]*AV_CODEC_ID_NONE`           | `[ \t]*AV_CODEC_ID_FIRST_AUDIO`    | `WRAPPED_AVFRAME`, `NONE` | 276     |
| `audio`    | `AudioCodecID`    | `[ \t]*AV_CODEC_ID_FIRST_AUDIO`    | `[ \t]*AV_CODEC_ID_FIRST_SUBTITLE` | `WRAPPED_AVFRAME`, `NONE` | 225     |
| `subtitle` | `SubtitleCodecID` | `[ \t]*AV_CODEC_ID_FIRST_SUBTITLE` | `[ \t]*AV_CODEC_ID_FIRST_UNKNOWN`  | `NONE`                    | 28      |
| `unknown`  | `UnknownCodecID`  | `[ \t]*AV_CODEC_ID_FIRST_UNKNOWN`  | none                               | `NONE`                    | 23      |
| `codec_id` | `CodecID`         | `[ \t]*AV_CODEC_ID_NONE`           | none                               | `NONE`                    | 550     |

Because the start line is consumed, `NONE` and the three `FIRST_*` markers
are absent from the ranges they open; `NONE` re-enters every block as an
extra. _Observed_ consequences:

- `video` and `audio` both start with `` `Wrapped_avframe ``, `` `None ``.
- `unknown` holds everything after the `FIRST_UNKNOWN` line: the data and
  attachment codecs, then `Probe`, `Mpeg2ts`, `Mpeg4systems`, `Ffmetadata`,
  `Wrapped_avframe`, `Vnull`, `Anull`.
- `codec_id` holds every member of the enum, including `` `First_audio ``,
  `` `First_subtitle `` and `` `First_unknown ``, each placed immediately
  before the real codec that has the same C value (`` `Pcm_s16le ``,
  `` `Dvd_subtitle ``, `` `Ttf ``).

Table `swresample_options`: header `libswresample/swresample.h`,
preprocessed, three blocks, all with C prefix `SWR_`:

| OCaml type    | Radix        | C type               | Start pattern           | Stop pattern              | Members (_observed_)                                                      |
| ------------- | ------------ | -------------------- | ----------------------- | ------------------------- | ------------------------------------------------------------------------- |
| `dither_type` | `DitherType` | `enum SwrDitherType` | `[ \t]*SWR_DITHER_NONE` | `[ \t]*SWR_DITHER_NS`     | `Dither_rectangular`, `Dither_triangular`, `Dither_triangular_highpass`   |
| `engine`      | `Engine`     | `enum SwrEngine`     | `enum SwrEngine`        | `[ \t]*SWR_ENGINE_NB`     | `Engine_swr`, `Engine_soxr`                                               |
| `filter_type` | `FilterType` | `enum SwrFilterType` | `enum SwrFilterType`    | a line starting with `};` | `Filter_type_cubic`, `Filter_type_blackman_nuttall`, `Filter_type_kaiser` |

`SWR_DITHER_NONE` is the consumed start line and is not an extra, so it is
not in `dither_type`. The noise-shaping dithers, from `SWR_DITHER_NS` on,
are after the stop line.

### 3.6 Output shapes

#### OCaml module (`<table-name>.ml`)

For each block, in order:

1. `type <type-name> = [`, then one line ` |` followed by a backquote and
   the constructor per member, in emission order (extras first, then scan
   order), then `]` and an empty line.
2. `let <type-name>: <type-name> list  = [` (two spaces before `=`), then
   one line per member holding a backquote, the constructor and `;`, in
   **reverse** emission order, then `]` and an empty line.

So each block defines a closed polymorphic variant type and a value of the
same name listing every constructor, last member first. A block whose start
pattern was not found defines nothing.

_Observed_, complete file `color_range.ml`:

```ocaml
type t = [
  | `Unspecified
  | `Mpeg
  | `Jpeg
]

let t: t list  = [
`Jpeg;
`Mpeg;
`Unspecified;
]

```

A multi-block file is the concatenation of such pairs (`type video`,
`let video`, `type audio`, `let audio`, …).

#### C header (`<table-name>_stubs.h`)

The file starts, once, with:

```c
#include "avutil_stubs.h"
#include <inttypes.h>

#define VALUE_NOT_FOUND 0xFFFFFFF

```

There is no include guard. Then, per block, with
`TAB = <C prefix><type name in upper case>_TAB`
(`AV_PIX_FMT_T_TAB`, `AV_CODEC_ID_VIDEO_TAB`, `SWR_DITHER_TYPE_TAB`,
`SUBTITLE_T_TAB`):

1. `static const int64_t TAB[][2] = {`, then one row
   `  {(<hash>), <C prefix><MEMBER>},` per member in emission order, then
   `};`. Column 0 is the OCaml representation of the constructor (3.7) as a
   decimal literal; column 1 is the C constant by name. No row carries an
   `#ifdef` or any other guard.
2. `#define TAB_LEN <number of members>`.
3. Three function definitions with external linkage (not `static`, not
   `inline`):

| Function                                 | Lookup                                                                                                      | No mapping                                                                                                                                                                                                                                                                  |
| ---------------------------------------- | ----------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<C type> <Radix>_val(value v)`          | Linear scan from row 0; returns column 1 of the first row whose column 0 equals `v`.                        | Raises ``Avutil.Error (`Failure msg)`` through the shared failure path ([avutil.md](avutil.md) §5.2), with the message `Could not find C value for <v as unsigned decimal> in TAB. Do you need to recompile the ffmpeg binding?`. Control does not come back to the caller. |
| `<C type> <Radix>_val_no_raise(value v)` | Same scan.                                                                                                  | Returns `VALUE_NOT_FOUND` (`0xFFFFFFF`, 268435455) converted to the C type.                                                                                                                                                                                                 |
| `value Val_<Radix>(<C type> t)`          | Linear scan from row 0; returns column 0 of the first row whose column 1 equals `t` converted to `int64_t`. | Raises ``Avutil.Error (`Failure msg)`` the same way, with `Could not find OCaml value for <t as unsigned decimal> in TAB. Do you need to recompile the ffmpeg binding?`.                                                                                                    |

Consequences of "first row wins" in the C-to-OCaml direction: when two
members share a C value, the one emitted first is always returned. Extras
are emitted first.

The failing functions raise through the shared failure mechanism declared
in `avutil_stubs.h` (see the avutil specification); they call into OCaml,
so they must be called with the runtime lock held. The `_no_raise` variant
and direct table access are pure C.

The table symbol and its length macro are part of the contract, not only
the functions: the avcodec stubs iterate over `AV_CODEC_CAP_T_TAB`,
`AV_CODEC_PROP_T_TAB`, `AV_CODEC_HW_CONFIG_METHOD_T_TAB`,
`AV_CODEC_ID_AUDIO_TAB`, `AV_CODEC_ID_VIDEO_TAB` and
`AV_CODEC_ID_SUBTITLE_TAB` directly, and the avutil stubs over
`AV_PIX_FMT_FLAG_T_TAB`, to turn a bit mask into a list of constructors or
to search several tables.

_Observed_, complete file `color_range_stubs.h`:

```c
#include "avutil_stubs.h"
#include <inttypes.h>

#define VALUE_NOT_FOUND 0xFFFFFFF

static const int64_t AVCOL_RANGE_T_TAB[][2] = {
  {(-789514449), AVCOL_RANGE_UNSPECIFIED},
  {(1718977867), AVCOL_RANGE_MPEG},
  {(1652440465), AVCOL_RANGE_JPEG},
};

#define AVCOL_RANGE_T_TAB_LEN 3
enum AVColorRange ColorRange_val(value v){
int i;
for(i=0;i<AVCOL_RANGE_T_TAB_LEN;i++){
if(v==AVCOL_RANGE_T_TAB[i][0])return AVCOL_RANGE_T_TAB[i][1];
}
Fail("Could not find C value for %" PRIu64 " in AVCOL_RANGE_T_TAB. Do you need to recompile the ffmpeg binding?", (uint64_t)v);
return -1;
}
enum AVColorRange ColorRange_val_no_raise(value v){
int i;
for(i=0;i<AVCOL_RANGE_T_TAB_LEN;i++){
if(v==AVCOL_RANGE_T_TAB[i][0])return AVCOL_RANGE_T_TAB[i][1];
}
return VALUE_NOT_FOUND;
}
value Val_ColorRange(enum AVColorRange t){
int i;
for(i=0;i<AVCOL_RANGE_T_TAB_LEN; i++){
if((int64_t)t==AVCOL_RANGE_T_TAB[i][1])return AVCOL_RANGE_T_TAB[i][0];
}
Fail("Could not find OCaml value for %" PRIu64 " in AVCOL_RANGE_T_TAB. Do you need to recompile the ffmpeg binding?", (uint64_t)t);
return -1;
}
```

_Observed_ variations: with C type `uint64_t` the signatures are
`uint64_t MediaTypes_val(value v)` and `value Val_MediaTypes(uint64_t t)`;
with C type `int`, `int SubtitleFlag_val(value v)`. A multi-block header
has the three-line prologue once and then the table, length and three
functions for each block, for example
`SWR_DITHER_TYPE_TAB` / `DitherType_val` / `DitherType_val_no_raise` /
`Val_DitherType`, then `SWR_ENGINE_TAB` / `Engine_val` / …, then
`SWR_FILTER_TYPE_TAB` / `FilterType_val` / ….

### 3.7 Variant constants in C

An OCaml constant polymorphic variant is an immediate integer. Its C
representation is computed from the constructor name `s` (without the
backquote):

1. `h = 0`; for each byte `c` of `s`, `h = 223 * h + c`.
2. Keep the low 31 bits of `h`.
3. The constant is `2 * h + 1`, interpreted as a signed 32-bit integer and
   sign-extended to the platform word.

The generator obtains this number by calling the OCaml runtime's own
variant-hash function on the name, from a one-function C stub linked into
the generator, and prints it in decimal. The printed number is directly
comparable, with `==`, to an OCaml value received by a stub, and can be
returned as an OCaml value without further encoding.

_Checked_ against the observed output by recomputation: `Jpeg` =
1652440465, `Unspecified` = -789514449, `Audio` = 1968951661, `Video` =
-1806497609, `None` = 1741061553, `_0rgb` = -1505465991.

### 3.8 The variant-values header

`gen_code <cc> polymorphic_variant h` writes
`polymorphic_variant_values_stubs.h`. It reads no FFmpeg header. For each
name `N` of a fixed list compiled into the generator it writes one line

```c
#define PVV_N (<constant of 3.7 for N>)
```

and nothing else: no include guard, no includes. _Observed_ first lines:

```c
#define PVV_Audio (1968951661)
#define PVV_Audio_frame (24258889)
#define PVV_Audio_packet (-1332263325)
#define PVV_Video (-1806497609)
```

The list is hand-maintained: it is the set of constructor names the
hand-written stubs need to build or recognise without a generated table.
It has 89 names, in this order:

- media and container kinds: `Audio`, `Audio_frame`, `Audio_packet`,
  `Video`, `Video_frame`, `Video_packet`, `Subtitle`, `Subtitle_frame`,
  `Subtitle_packet`, `Data`, `Data_packet`, `Attachment`, `Nb`, `Packet`,
  `Frame`, `Ok`, `Again`;
- time formats: `Second`, `Millisecond`, `Microsecond`, `Nanosecond`;
- filter graph: `Buffer`, `Link`, `Sink`;
- filter flags: `Dynamic_inputs`, `Dynamic_outputs`, `Slice_threads`,
  `Support_timeline_generic`, `Support_timeline_internal`;
- packet flags: `Keyframe`, `Corrupt`, `Discard`, `Trusted`, `Disposable`;
- packet side data: `Replaygain`, `Strings_metadata`, `Metadata_update`;
- option types and flags: `Constant`, `Flags`, `Int`, `Int64`, `Float`,
  `Double`, `String`, `Rational`, `Binary`, `Dict`, `UInt64`, `Image_size`,
  `Pixel_fmt`, `Sample_fmt`, `Video_rate`, `Duration`, `Color`,
  `Channel_layout`, `Bool`, `Array`, `Encoding_param`, `Decoding_param`,
  `Audio_param`, `Video_param`, `Subtitle_param`, `Export`, `Readonly`,
  `Bsf_param`, `Runtime_param`, `Filtering_param`, `Deprecated`,
  `Child_consts`;
- subtitle flags: `Forced`;
- errors: `Bsf_not_found`, `Decoder_not_found`, `Demuxer_not_found`,
  `Encoder_not_found`, `Eof`, `Exit`, `Filter_not_found`, `Invalid_data`,
  `Muxer_not_found`, `Option_not_found`, `Patch_welcome`,
  `Protocol_not_found`, `Stream_not_found`, `Bug`, `Eagain`, `Unknown`,
  `Experimental`, `Other`, `Failure`.

A stub that uses a `PVV_` name absent from the list does not compile. The
names are used by the avutil, avcodec, av and avfilter stubs.

## 4. Generated-file wiring

Each table is generated inside the directory of the library that owns it,
with that library's detected C flags on the generator command line.

| Library                                 | Generated OCaml modules                                                                                                                                                                                                    | Generated C headers                                                          |
| --------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| `avutil`                                | `Hw_device_type`, `Media_types`, `Color_space`, `Color_range`, `Color_primaries`, `Color_trc`, `Chroma_location`, `Pixel_format`, `Pixel_format_flag`, `Sample_format`, `Channel_layout`, `Subtitle_type`, `Subtitle_flag` | the matching thirteen `*_stubs.h`, plus `polymorphic_variant_values_stubs.h` |
| `avcodec`                               | `Hw_config_method`, `Codec_capabilities`, `Codec_properties`, `Codec_id`                                                                                                                                                   | the matching four `*_stubs.h`                                                |
| `swresample`                            | `Swresample_options`                                                                                                                                                                                                       | `swresample_options_stubs.h`                                                 |
| `av`, `avfilter`, `avdevice`, `swscale` | none                                                                                                                                                                                                                       | none                                                                         |

Which stub file includes which generated header:

| Stub file of                | Includes                                                                                                                      |
| --------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `avutil`                    | twelve of its own table headers (all except `media_types_stubs.h`); the variant-values header through its hand-written header |
| `avcodec`                   | its own four table headers and avutil's `media_types_stubs.h`                                                                 |
| `swresample`                | `swresample_options_stubs.h`                                                                                                  |
| `avfilter`                  | the variant-values header                                                                                                     |
| `av`, `avdevice`, `swscale` | no generated table header directly; the variant-values header through avutil's hand-written header                            |

Because the generated functions have external linkage and the headers have
no include guard, each table header is included by exactly one stub file in
the whole set of libraries. Sibling stubs call the conversion functions
through prototypes written by hand in `avutil_stubs.h` (sample format, the
five colour enums, pixel format, hardware device type) and in
`avcodec_stubs.h` (the four per-range codec id converters). No prototype is
exported for `CodecID_val`/`Val_CodecID`, the media type, the flag tables
or the swresample tables.

The `channel_layout` table's C functions are generated and compiled but no
stub calls them; only the OCaml type and list are used.

Headers installed for downstream C code:

| Package          | Installed headers                                                             |
| ---------------- | ----------------------------------------------------------------------------- |
| `ffmpeg-avutil`  | `avutil_stubs.h`, `polymorphic_variant_values_stubs.h`, `media_types_stubs.h` |
| `ffmpeg-avcodec` | `avcodec_stubs.h`                                                             |
| `ffmpeg-av`      | `av_stubs.h`                                                                  |

No other generated header is installed, and neither is the hand-written
swresample header.

Each of the three libraries with generated headers also carries a rule
that names the hand-written stub source as its target, in fallback mode,
with the generated headers as dependencies and an action that only prints
`this should not happen`. Since the stub source exists in the source tree,
the rule never runs. It records the list of generated headers the stub
needs; the actual ordering (headers generated before the stub is compiled)
comes from the build system, which makes a stub's compilation depend on
every header of its directory. See the language notes.

## 5. Tests and examples build mechanism

Two small generator programs print dune stanzas on standard output. Each
receives the content of `all_libs_available` (2.9) as its only argument.
When the argument is exactly `true` it prints its stanzas; otherwise it
prints nothing. The output is stored as `test_executables.inc` and
`examples_executables.inc` and included dynamically by the build
descriptions of the test and example directories. With any library missing,
both files are empty and no test or example exists in the build.

Examples generator: one executable stanza per example, each with a single
module of the same name and its own library list.

| Libraries                                           | Examples                                                                                                           |
| --------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `ffmpeg-av`                                         | `hw_encode`, `encode_video`, `read_metadata`, `remuxing`, `transcoding`, `decoding`, `interrupt`, `subtitle_remux` |
| `ffmpeg-av`, `ffmpeg-avfilter`                      | `aresample`, `decode_stream`, `fps`, `fps_samplerate`, `transcode_aac`                                             |
| `ffmpeg-av`, `ffmpeg-swresample`                    | `audio_decoding`, `encoding`                                                                                       |
| `ffmpeg-av`, `ffmpeg-avfilter`, `ffmpeg-swresample` | `encode_stream`                                                                                                    |
| `ffmpeg-av`, `ffmpeg-swresample`, `ffmpeg-swscale`  | `demuxing_decoding`                                                                                                |
| `ffmpeg-av`, `ffmpeg-avdevice`                      | `player`, `webcam`                                                                                                 |
| `ffmpeg-av`, `ffmpeg-swscale`                       | `bitmap_subtitle_to_jpeg`                                                                                          |
| `ffmpeg-avcodec`, `ffmpeg-swresample`               | `encode_audio`                                                                                                     |
| `ffmpeg-av`, `ffmpeg-avcodec`                       | `reenc_vp9`                                                                                                        |
| `ffmpeg-avcodec`                                    | `all_codecs`, `all_bitstream_filters`                                                                              |
| `ffmpeg-avutil`                                     | `all_channel_layouts`                                                                                              |
| `ffmpeg-avfilter`                                   | `list_filters`, `filter_info`                                                                                      |
| `ffmpeg-avdevice`                                   | `audio_device`                                                                                                     |

In the example directory,
compiler warnings never fail the build in the development profile.

Tests generator: one stanza declaring eleven executables at once
(`test_resample`, `test_info`, `test_subtitle_read`,
`test_unhandled_packet`, `test_seek`, `test_drain`,
`test_input_streams_grow`, `test_codec`, `test_options`, `test_swscale`,
`test_swresample`), sharing the helper module `test_assert`, all linked
with `ffmpeg-av`, `ffmpeg-swresample`, `ffmpeg-swscale`. It is one stanza
because the shared module can belong to only one stanza.

Then one rule attached to the alias `ffmpeg_citest` and to the package
`ffmpeg`. Its dependencies are the test runner, the eleven test
executables, twenty-one example executables, a line-ending normaliser and one
subtitle fixture file. Its action is one strict sequence: each step is
either the runner wrapping one executable with arguments, a direct call to
the `ffmpeg` command-line tool to synthesise an input file, or a `diff`.
Later steps consume files written by earlier ones, so the order is part of
the rule. The first failing step stops the sequence and fails the alias.
The `ffmpeg` executable must be in `PATH`.

The test runner and the line-ending normaliser are always built, whatever
the availability. The runner is called as
`test_runner <name> <command> [args...]`. It runs `./<command>` with the
arguments, inheriting standard streams, prints a heading before and a
`PASSED: <name>` or `FAILED: <name> (exit code <n>)` line after, and exits
with the child's exit code (128 plus the signal number when killed by a
signal). Under `GITHUB_ACTIONS` it frames the output with `::group::` /
`::endgroup::` and adds an `::error::` line on failure.

The tests are reachable only through the `ffmpeg_citest` alias. The
standard test alias has nothing attached.

## 6. Packaging and CI

The opam files are generated from the project description into an `opam`
directory; a template per package appends `x-maintenance-intent:
["(latest)"]`, and for the seven binding packages a failure message
pointing at the README for the minimum FFmpeg version.
`ffmpeg-avutil` also lists distributions whose CI failures are accepted.
Version substitution at release time is disabled; the version is the
literal in the project description.

The README states FFmpeg 5.1 or later and "the last two major releases
supported". The enforced minimums are the pkg-config versions of 2.3.

Continuous integration, as described by the workflow files in this
directory:

- **Build and test**: on pull requests, merge groups and pushes to `main`.
  Matrix: macOS and Ubuntu, OCaml 4.14 and 5.3, FFmpeg `n8.0.1`; plus
  Ubuntu, OCaml 4.14, FFmpeg `n7.1`. FFmpeg is built from source: a shallow
  clone of the tag from `git.ffmpeg.org`, configured with shared libraries,
  GPL and nonfree enabled, x86 assembly, documentation and Xlib disabled,
  and libtheora, libvpx, libmp3lame, libvorbis and libsoxr enabled;
  installed under `/usr` on Linux and `/usr/local` on macOS. The built tree
  is cached per action-file hash, version and OS. The module is then built
  and tested with a shared savonet action, and finally
  `dune build @ffmpeg_citest` runs.
- **Documentation**: on pushes to `main`, installs the `ffmpeg` package
  with documentation, builds the API documentation and publishes it to the
  `gh-pages` branch.
- **Release**: on a `vX.Y.Z` tag, extracts that version's section from the
  `CHANGES` file (the lines after the heading that starts with the version,
  minus the `===` underline, up to the next version heading, trimmed of
  blank lines at both ends), fails when it is empty, and creates or updates
  the GitHub release named after the tag with the body
  `## ocaml-ffmpeg (<version>)` followed by the extracted notes. Publishing
  to opam is not part of the workflow.

`CHANGES` is merged with the union strategy.

### Standalone repository versus embedded tree

This tree lives inside the liquidsoap repository and is mirrored to the
standalone `savonet/ocaml-ffmpeg` repository by a liquidsoap workflow that
copies the directory over a clone of the standalone repository and pushes
to its `main` branch. The README says the standalone repository is
read-only for that reason.

Meaningful only in the standalone repository:

- the three workflows, the FFmpeg build action and the changelog script
  (workflows in a subdirectory do not run);
- the `ffmpeg_citest` alias, which nothing in the enclosing tree's build
  or CI descriptions references;
- the release mechanics and the documentation deployment;
- the README's installation instructions and example links;
- the submodule declaration for an `m4` directory that does not exist in
  the tree and that nothing uses.

Meaningful only when embedded in liquidsoap:

- the three `LIQUIDSOAP_*` variables. The enclosing workspace sets
  `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL=true` for its default context, so
  the install-time check of 2.7 is off there and a missing FFmpeg library
  silently disables the corresponding binding. The Windows cross build
  sets `LIQUIDSOAP_DUNE_TARGET=default.windows`. The minimal-dependency
  builds set `LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS`.

The project description, its eight packages and the generated opam files
serve both: the enclosing project lists `ffmpeg` as an optional dependency
and conflicts with `ffmpeg` and `ffmpeg-avutil` below 1.4.0.
