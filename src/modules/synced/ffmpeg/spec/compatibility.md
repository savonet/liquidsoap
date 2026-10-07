# FFmpeg version compatibility

The set of FFmpeg versions the bindings build and run against. This is frozen
surface: a rewrite supports the same range.

Each library file's §10 describes the behaviour on each side of a conditional.
This file owns the range itself and the list of conditionals.

## 1. Supported range

### 1.1 Lower bound

A binding library is built only when pkg-config finds the C libraries it needs
at a minimum version. The requirement of each library is an argument of its
detection rule: [build.md](build.md) §2.3 owns the table.

The project README states the bound as "FFmpeg 5.1 or later" and adds that only
the last two major releases are intended to be supported.

The bindings use the `AVChannelLayout` structure API throughout
(`frame->ch_layout`, `av_channel_layout_*`) and have no code path for the
earlier `uint64_t` channel-layout masks.

### 1.2 Upper bound

There is no upper bound in the build. The versions exercised are:

| Where                         | FFmpeg                                                                                                                                                                   |
| ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| CI, Linux and macOS           | tag `n8.0.1`, OCaml 4.14 and 5.3                                                                                                                                         |
| CI, Linux only                | tag `n7.1`, OCaml 4.14                                                                                                                                                   |
| This snapshot's build machine | development head: libavutil 61.7.100, libavcodec 63.14.100, libavformat 63.7.100, libavfilter 12.4.100, libavdevice 63.2.100, libswscale 10.2.100, libswresample 7.3.100 |

No FFmpeg 5.x or 6.x release is built in CI.

Checked for this snapshot, on Linux aarch64 with OCaml 5.5.0, against release
builds configured with `libmp3lame`, `libvpx` and `libvorbis` and no other
external library:

| FFmpeg | All seven libraries detected | Bindings compile | Test steps passing |
| ------ | ---------------------------- | ---------------- | ------------------ |
| 7.1.5  | yes                          | yes              | 42 of 45           |
| 8.1.3  | yes                          | yes              | 42 of 45           |
| 9.0.2  | yes                          | yes              | 42 of 45           |

The same three steps fail on each, for reasons in the FFmpeg build and not in
the bindings: two request the `soxr` resampling engine and one decodes a PNG,
and these builds have neither `libsoxr` nor `zlib`. The media fixtures the
later steps read were produced by the `ffmpeg` tool of the development head.
[tests.md](tests.md) lists what each step needs.

Against 5.1.10, built without external libraries, all seven libraries are
detected and the bindings compile. The first test step fails there: it expects
the native AAC encoder to report the `` `Dr1 `` capability, which it does not
in that release. No later step was run on 5.1.

### 1.3 Enumerations follow the headers

Enum tables are generated from the installed FFmpeg headers at build time
([build.md](build.md) §3). The set of constructors of each generated
polymorphic variant type therefore depends on the FFmpeg version the bindings
were built against. This is part of the compatibility contract: a program that
names a constructor absent from the installed headers fails to compile.

## 2. Conditionals that select behaviour inside the range

Every one of these is live: both sides are reachable for some version at or
above the lower bound.

| #   | Library    | Condition              | What it selects                                   | Behaviour                      |
| --- | ---------- | ---------------------- | ------------------------------------------------- | ------------------------------ |
| 1   | `avutil`   | libavutil ≥ 57.30.100  | whether frames carry a duration                   | [avutil.md](avutil.md) §10     |
| 2   | `avutil`   | libavutil ≥ 59.1.100   | whether array-typed options exist                 | [avutil.md](avutil.md) §10     |
| 6   | `avcodec`  | libavcodec < 60.26.100 | the names of the unknown profile and level        | [avcodec.md](avcodec.md) §10   |
| 7   | `avcodec`  | libavcodec ≤ 61.13.100 | where a codec's supported configurations are read | [avcodec.md](avcodec.md) §10   |
| 9   | `av`       | libavformat major < 61 | the buffer type of the custom write function      | [avformat.md](avformat.md) §10 |
| 10  | `av`       | libavcodec ≥ 60.15.100 | where a stream's bitrate fallback is read         | [avformat.md](avformat.md) §10 |
| 11  | `avfilter` | libavutil ≥ 59.1.100   | whether the array-separator lookup can succeed    | [avfilter.md](avfilter.md) §10 |

## 3. Conditionals that take one side across the whole range

Seven test a version below the lower bound and always take the same side for
any version that passes detection. They are recorded so the snapshot is complete.

| Library      | Condition               | Side always taken                                                                              |
| ------------ | ----------------------- | ---------------------------------------------------------------------------------------------- |
| `avcodec`    | libavcodec < 58.9.100   | False: `avcodec_register_all` is not called.                                                   |
| `av`         | libavcodec < 58.9.100   | False: `av_register_all` is not called.                                                        |
| `av`         | libavformat ≤ 59.0.100  | False: input and output format pointers are `const`.                                           |
| `avfilter`   | libavfilter < 7.14.100  | False: filters are enumerated with `av_filter_iterate`; `avfilter_register_all` is not called. |
| `avfilter`   | libavfilter < 8.3.100   | False: pad counts come from `avfilter_filter_pad_count`.                                       |
| `swresample` | libavcodec < 56.0.100   | False.                                                                                         |
| `swresample` | libavformat < 59.19.100 | False: a frame's channel count is `frame->ch_layout.nb_channels`.                              |

Four conditionals test a macro that every release from 5.1 to 9.0 defines, so
they always take the same side:

| Library   | Condition                            | Side always taken                               |
| --------- | ------------------------------------ | ----------------------------------------------- |
| `avutil`  | `AV_OPT_FLAG_BSF_PARAM` defined      | True: the `` `Bsf_param `` flag maps to it.     |
| `avutil`  | `AV_OPT_FLAG_DEPRECATED` defined     | True: the `` `Deprecated `` flag maps to it.    |
| `avutil`  | `AV_OPT_FLAG_RUNTIME_PARAM` defined  | True: the `` `Runtime_param `` flag maps to it. |
| `avcodec` | `AV_PKT_FLAG_DISPOSABLE` not defined | False: FFmpeg's own definition is used.         |

One conditional always takes the same side for every version:

| Library  | Condition                                      | Side always taken                              |
| -------- | ---------------------------------------------- | ---------------------------------------------- |
| `avutil` | `AV_OPT_FLAG_AV_OPT_FLAG_CHILD_CONSTS` defined | False: the `` `Child_consts `` flag maps to 0. |

See [findings.md](findings.md).

## 4. Library versions per FFmpeg release

Read from the version headers at each release tag.

| Release | libavutil | libavcodec | libavformat | libavfilter | libavdevice | libswscale | libswresample |
| ------- | --------- | ---------- | ----------- | ----------- | ----------- | ---------- | ------------- |
| 5.1     | 57.28.100 | 59.37.100  | 59.27.100   | 8.44.100    | 59.7.100    | 6.7.100    | 4.7.100       |
| 6.0     | 58.2.100  | 60.3.100   | 60.3.100    | 9.3.100     | 60.1.100    | 7.1.100    | 4.10.100      |
| 6.1     | 58.29.100 | 60.31.102  | 60.16.100   | 9.12.100    | 60.3.100    | 7.5.100    | 4.12.100      |
| 7.0     | 59.8.100  | 61.3.100   | 61.1.100    | 10.1.100    | 61.1.100    | 8.1.100    | 5.1.100       |
| 7.1     | 59.39.100 | 61.19.100  | 61.7.100    | 10.4.100    | 61.3.100    | 8.3.100    | 5.3.100       |
| 8.0     | 60.8.100  | 62.11.100  | 62.3.100    | 11.4.100    | 62.1.100    | 9.1.100    | 6.1.100       |
| 8.1     | 60.26.100 | 62.28.100  | 62.12.100   | 11.14.100   | 62.3.100    | 9.5.100    | 6.3.100       |
| 9.0     | 61.1.100  | 63.1.100   | 63.1.100    | 12.1.100    | 63.1.100    | 10.1.100   | 7.1.100       |

Every pkg-config requirement of §1.1 is met by 5.1 and by no 5.0 release
(5.0 ships libavutil 57.17.100).

The side each version conditional of §2 takes, per release. "new" is the
side taken by the newest release: the condition is true for 1, 2, 10 and 11
and false for 6, 7 and 9.

| #     | Threshold            | 5.1 | 6.0 | 6.1 | 7.0 | 7.1 | 8.0 | 8.1 | 9.0 |
| ----- | -------------------- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1     | libavutil 57.30.100  | old | new | new | new | new | new | new | new |
| 2, 11 | libavutil 59.1.100   | old | old | old | new | new | new | new | new |
| 6     | libavcodec 60.26.100 | old | old | new | new | new | new | new | new |
| 7     | libavcodec 61.13.100 | old | old | old | old | new | new | new | new |
| 9     | libavformat major 61 | old | old | old | new | new | new | new | new |
| 10    | libavcodec 60.15.100 | old | old | new | new | new | new | new | new |

From 7.1 onward every version conditional in the bindings takes one side.
Between 7.0 and 7.1 exactly one differs: where a codec's supported
configurations are read from (conditional 7).

## 5. OCaml versions

| Where                         | OCaml           |
| ----------------------------- | --------------- |
| Package metadata              | `ocaml >= 4.12` |
| CI                            | 4.14 and 5.3    |
| This snapshot's build machine | 5.5.0           |
| Build system                  | dune 3.23       |

The bindings contain these provisions for older OCaml runtimes:

| Provision                                                                  | Needed below |
| -------------------------------------------------------------------------- | ------------ |
| A fallback definition of the `bytes` data accessor in two stub files       | OCaml 4.06   |
| A fallback definition of the option payload accessor in the shared header  | OCaml 4.12   |
| A private definition of the `None` immediate in the shared header          | OCaml 4.12   |
| An explicit request for the name-spaced C API at the top of each stub file | OCaml 5.0    |

The OCaml side uses `Atomic`, `Mutex` and `Thread` from the standard
distribution and nothing specific to OCaml 5: no domains, no effects.
