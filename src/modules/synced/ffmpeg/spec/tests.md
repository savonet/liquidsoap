# Conformance

What a test suite for a conforming implementation must assert, and what it
must not depend on.

## 0. Principles

- **A requirement is an assertion.** Each entry below names a value to
  compare or an error to expect. A step that only runs a call and passes when
  nothing crashes satisfies no requirement.
- **Binding and incidental.** For each requirement the binding part is
  stated. Everything else is incidental and a suite MUST NOT assert it: the
  wording of messages (except the three texts of
  [binding-contract.md](binding-contract.md) §5.3), log content, the order of
  unordered results, file names, timing, the representation of handles.
- **Misuse is tested like use.** For every operation that exists once per
  media kind, the same misuse cases run on every kind (§12, "One operation
  written once per media kind").
- **Every known pitfall is a check.** §12.
- **A check must be able to fail.** §10.

Requirements are numbered per section. "Rule" columns point at the rule a
requirement verifies.

## 1. avutil

| #    | Requirement (binding part)                                                                                                                                                                                                                                                             | Rule                         |
| ---- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------- |
| 1.1  | For every constructor `v` of each of the five colour enumerations that FFmpeg names, `from_name (name v)` is `Some` of a value with the same C value. An unknown name gives `None`.                                                                                                    | [avutil.md](avutil.md) §4.10 |
| 1.2  | `Pixel_format` and `Sample_format`: every constructor converts to C and back to itself or to its first-declared alias. `find_id` and `find` raise `Not_found` on a miss.                                                                                                               | contract E2–E5; avutil §5.3  |
| 1.3  | An option read through `Options` equals the value set through FFmpeg: set a container option to a chosen non-default value at open, read it back with each getter of matching type.                                                                                                    | avutil §4.16; contract B2    |
| 1.4  | `?search_children:false` and omitting it do not search child objects; `true` does: an option that exists only on a child is found in the third case alone.                                                                                                                             | avutil §4.16                 |
| 1.5  | `Options.opts` on the option class of every registered codec, format, filter and bitstream filter returns. For an option with named constants, `values` lists them.                                                                                                                    | avutil §4.15                 |
| 1.6  | The bounds of an option spanning the full 64-bit range are the two 64-bit extremes.                                                                                                                                                                                                    | avutil §3.2                  |
| 1.7  | Option tables: after each operation that takes `?opts`, given one valid and one unknown key, the table holds the unknown key only. After a failing operation it is unchanged. 20000 unknown keys all come back intact.                                                                 | avutil §9.1; contract F7     |
| 1.8  | `Frame.set_pts` then every timestamp accessor: they agree.                                                                                                                                                                                                                             | avutil §4.3                  |
| 1.9  | `Frame.set_metadata` with `{a, b}` then `{b}`: `metadata` returns `{b}`.                                                                                                                                                                                                               | avutil §4.3                  |
| 1.10 | `Video.frame_visit` on 4:2:0 and 4:2:2 frames: each plane's length is its line size times that plane's height. A bigarray kept after the frame is dropped and collected is still readable and writable.                                                                                | avutil §8.2; contract L4     |
| 1.11 | `Subtitle`: for text and bitmap contents, `get_content (create_frame c) = c`. Plane arrays of a length other than 4 and planes of a wrong size raise.                                                                                                                                  | avutil §4.14                 |
| 1.12 | `Channel_layout`: a standard and a custom layout round-trip through every operation that returns a layout; `find` of an unknown name and `get_default 0` raise `Not_found`; `get_default` of a count with no standard layout has that many channels and no mask.                       | avutil §2.2, §4.8            |
| 1.13 | Logging, with a callback installed and the level at debug, while several threads encode: every message is delivered once, in order per logging thread, and none after `clear_callback` returned.                                                                                       | avutil §7.1 N5–N8            |
| 1.14 | Logging: a message logged from a thread that holds the runtime lock is delivered and nothing hangs. `set_callback` immediately after `clear_callback` delivers to the new callback. A callback that raises does not stop delivery. A callback that calls `set_callback` does not hang. | avutil §7.1 N1, N9, N10      |
| 1.15 | `Error`: each constructor of the mapping table is raised for its FFmpeg code and `string_of_error` returns FFmpeg's text for it.                                                                                                                                                       | avutil §5.1                  |

## 2. avcodec

| #    | Requirement (binding part)                                                                                                                                                                                                                               | Rule                         |
| ---- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------- |
| 2.1  | The name lists of `Audio`, `Video` and `Subtitle` `encoders` and `decoders` equal, as sets, the codecs of that kind and direction FFmpeg registers whose identifier is in the family. `get_id` succeeds on each.                                         | [avcodec.md](avcodec.md) §11 |
| 2.2  | For each family, every identifier converts to C and back to itself; the identifier of a codec found by identifier is the identifier asked for, compared as values.                                                                                       | contract E2–E5               |
| 2.3  | Each `get_supported_*` list holds exactly the entries `avcodec_get_supported_config` counts, for a codec that declares each kind of list.                                                                                                                | avcodec §4.3                 |
| 2.4  | `capabilities` of a codec matches a literal constructor for a capability the codec is known to have, for an encoder and for a decoder.                                                                                                                   | avcodec §4.1; contract B2    |
| 2.5  | A decoder uses several threads on a machine that has several, observed from outside the interface while it decodes with a codec that supports frame threading. An encoder given `threads` = 1 uses one.                                                  | avcodec §4.9                 |
| 2.6  | Decoding a stream with reordering returns as many frames as it has, the flush included. A packet that produces several frames delivers them all; one that produces none is not an error.                                                                 | avcodec §4.10, §11           |
| 2.7  | State table of §2.4, for a decoder and for an encoder: flush twice; send after flush; each has the documented outcome.                                                                                                                                   | avcodec §2.4                 |
| 2.8  | A user function that raises mid-delivery: the exception propagates unchanged, ``Error `Eof`` included; the next call delivers the frames that were pending.                                                                                              | avcodec §7.2                 |
| 2.9  | Packets: `set_flags` then `get_flags`; each setter then its getter, with the sentinels; `dup` shares the payload and copies the properties; a packet given to a decoder, a bitstream filter or a muxer is unchanged afterwards.                          | avcodec §4.2; contract A4    |
| 2.10 | Side data: each of the three kinds round-trips with its content and order. A metadata entry written by the binding is accepted by `av_packet_unpack_dictionary`. A ReplayGain peak outside 32 unsigned bits is rejected and leaves the packet unchanged. | avcodec §3.2                 |
| 2.11 | Bitstream filter: an option of the filter's private class given to `init` takes effect and is not reported unused.                                                                                                                                       | avcodec §4.8                 |
| 2.12 | Encoder typed arguments: after `Video.create_encoder ~frame_rate`, the parameters and the encoded stream carry that frame rate; an unsupported sample format, pixel format or size makes creation fail.                                                  | avcodec §4.9; contract O2    |
| 2.13 | Hardware upload: the timestamp of a frame encoded through a hardware frame context reaches the packets. Requires a device; reported as skipped without one (§10).                                                                                        | avcodec §4.10                |

## 3. av

| #    | Requirement (binding part)                                                                                                                                                                                                                                                                                                         | Rule                             |
| ---- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------- |
| 3.1  | Every container and stream operation on a closed container raises the closed error. `close` twice returns. A closed container dropped and collected does nothing.                                                                                                                                                                  | [avformat.md](avformat.md) §2.1  |
| 3.2  | Every open function, failed by each of its causes (unknown format, rejected option, unreachable URL, failed probe, raising configure function) with every callback supplied, then a full collection: no crash, no leak.                                                                                                            | contract L2, L8                  |
| 3.3  | A stream creation that fails for an unsupported parameter or a rejected option leaves the output usable: other streams can be written and `close` returns.                                                                                                                                                                         | avformat §2.1, §4.4              |
| 3.4  | An output opened with streams, then flushed and closed with nothing written: both return.                                                                                                                                                                                                                                          | avformat §2.1                    |
| 3.5  | `close` on an output whose final write fails: the error is raised and the container is closed.                                                                                                                                                                                                                                     | contract L6                      |
| 3.6  | Frame mode: a file with reordered frames yields exactly its frame count before ``Error `Eof``; after a seek to the start it yields the same count again. A one-frame image yields one frame.                                                                                                                                       | avformat §4.3                    |
| 3.7  | After reading some frames and seeking far ahead, the first frame returned has a timestamp at the target, within a stated tolerance on both sides.                                                                                                                                                                                  | avformat §4.3                    |
| 3.8  | A demuxer that adds streams while reading: the stream lists grow; a seek and a close afterwards succeed.                                                                                                                                                                                                                           | avformat §2.2                    |
| 3.9  | Selections: a stream in the packet selection yields packets, in the frame selection frames, in both packets; unselected streams reach `on_unhandled_packet`, typed by kind, for every kind present.                                                                                                                                | avformat §4.3                    |
| 3.10 | A frame-mode stream whose decoder fails to open: the read raises; a later read raises again or succeeds; `close` returns.                                                                                                                                                                                                          | avformat §4.3                    |
| 3.11 | Preferred decoder and per-stream options from a configure function are the ones the stream's decoder is opened with.                                                                                                                                                                                                               | avformat §4.3                    |
| 3.12 | Custom write: muxing through a write closure, at a bit rate that fills FFmpeg's buffer, with a closure that consumes a random part of each call, gives the bytes of the same muxing to a file.                                                                                                                                     | avformat §7.1.2                  |
| 3.13 | Custom read: an input read through closures decodes like the file; a read that returns 0 ends the input; a read that returns more than asked fails the operation without writing past the buffer.                                                                                                                                  | avformat §7.1.1                  |
| 3.14 | Each of the four closures raising: the enclosing operation raises `Avutil.Error`, the exception text is logged, and the container can be closed.                                                                                                                                                                                   | contract C1                      |
| 3.15 | Interrupt: an open blocked on a silent endpoint returns with ``Error `Exit`` once the function returns true, with collections forced meanwhile. The function is never called after `close` returned.                                                                                                                               | avformat §7.1.4                  |
| 3.16 | Key frames: with `on_keyframe` recording `tell`, every recorded position is the start of a key packet. The callback may call `flush`.                                                                                                                                                                                              | avformat §4.4, §6.2              |
| 3.17 | Options of `open_output`: one generic container option, one muxer private option and one protocol option all take effect and none is reported unused.                                                                                                                                                                              | avformat §9                      |
| 3.18 | Metadata set twice on a container and on a stream: the second list replaces the first.                                                                                                                                                                                                                                             | avformat §4.4                    |
| 3.19 | Remuxing a constant-rate video with `new_stream_copy` plus the frame-rate accessor pair: the output stream reports the source's frame rate. Without the pair it reports none.                                                                                                                                                      | avformat §4.4                    |
| 3.20 | Durations and aspect ratios: a live input reports no duration; a stream that declares no aspect ratio reports none.                                                                                                                                                                                                                | avformat §4.3                    |
| 3.21 | A duration or seek target beyond 3 hours in nanoseconds converts exactly.                                                                                                                                                                                                                                                          | avformat §4.3, "Time conversion" |
| 3.22 | `tell` beyond 2 GiB returns the exact position.                                                                                                                                                                                                                                                                                    | avformat §4.4                    |
| 3.23 | Subtitles: text subtitles read as frames, rebuilt from their content, encoded and muxed give the same cues, text and timing to the millisecond.                                                                                                                                                                                    | avformat §4.4                    |
| 3.24 | `codec_attr` for H.264, HEVC with and without an in-band SPS, and AAC equals the string FFmpeg's own HLS muxer writes for the same stream.                                                                                                                                                                                         | avformat §4.4                    |
| 3.25 | `write_packet` and `write_frame` on streams of the wrong mode, `get_frame_size` on a copy stream, `open_output_format` on a file format, a stream of another container: each raises a failure.                                                                                                                                     | avformat §2.2                    |
| 3.26 | A custom read function and an interrupt function called from a thread created in C complete, and the thread exits cleanly. The header services of avformat §1: a guard taken from C makes an operation raise the in-use error until it is released; a format wrapped from C names the same format; a null format raises a failure. | contract M10                     |

## 4. avfilter

| #    | Requirement (binding part)                                                                                                                                                            | Rule                          |
| ---- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------- |
| 4.1  | The module loads against an FFmpeg built with every optional filter, and against one configured for small size.                                                                       | contract I1                   |
| 4.2  | A graph built by `attach` and `link`, and the same graph built by `parse`, each run frames through: every sink receives its own stream, with the frame count and timestamps expected. | [avfilter.md](avfilter.md) §4 |
| 4.3  | `parse` with two labelled inputs and two labelled outputs between attached sources and sinks.                                                                                         | avfilter §4.10                |
| 4.4  | A failed `parse`, then every operation on the graph and on each handle obtained before: each raises the failed error or succeeds; none crashes.                                       | avfilter §2.1                 |
| 4.5  | A sink `context`, an attached pad and an attached filter, each kept alone while every other reference to the graph is dropped and a full collection runs: each is still usable.       | contract L3                   |
| 4.6  | Arguments are given in list order: a positional value before a pair is accepted, after it rejected by FFmpeg.                                                                         | avfilter §11.2                |
| 4.7  | `get_array_separator` returns `,` for an array option that declares none and the declared one otherwise; an array argument to such an option is accepted.                             | avfilter §4.3                 |
| 4.8  | `process_command` with `` `Fast `` has an effect that only that flag causes; on a hand-built filter record it raises.                                                                 | avfilter §2.4, §3             |
| 4.9  | `launch` twice, `attach` after `launch`, a duplicate instance name: each raises as stated.                                                                                            | avfilter §2.1, §4.7           |
| 4.10 | The audio converter delivers every input sample once: total samples out equal total samples in, scaled by the rate ratio, flush included.                                             | avfilter §11.3                |

## 5. avdevice

| #   | Requirement (binding part)                                                                                                           | Rule                          |
| --- | ------------------------------------------------------------------------------------------------------------------------------------ | ----------------------------- |
| 5.1 | A program that links the library and only looks up, with `Format.find_input_format`, a device format the FFmpeg build has, finds it. | [avdevice.md](avdevice.md) §1 |

## 6. swresample

| #   | Requirement (binding part)                                                                                                                                                        | Rule                                |
| --- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------- |
| 6.1 | Identity conversion for every output kind, with one, two and six channels and for each sample format: sample values equal the input within the format's precision.                | [swresample.md](swresample.md) §4.4 |
| 6.2 | `offset` and `length` on every input kind select exactly that range: compare with a conversion of the same range cut by hand.                                                     | swresample §4.4                     |
| 6.3 | A rate conversion followed by `flush`: the total number of samples is the input count scaled by the rate ratio. A second `flush` is empty.                                        | swresample §4.4                     |
| 6.4 | Two conversions with different input: the first result still equals a copy taken before the second call, for every output kind.                                                   | swresample §2.2                     |
| 6.5 | NaN in a float array input, and NaN produced into a float array output, are 0.                                                                                                    | swresample §4.4                     |
| 6.6 | Invalid input: wrong plane count, planes of unequal length, negative or oversized range, a frame of another format or channel count: each raises and reads nothing out of bounds. | swresample §4.4; contract A1        |
| 6.7 | `create`: a missing, conflicting or planar-for-bytes sample format raises; `from_codec` with a data kind of another format raises at creation.                                    | swresample §4.4                     |
| 6.8 | Every element of an options list longer than three is applied.                                                                                                                    | swresample §4.4                     |
| 6.9 | A custom-order layout on each side: created, used and collected with no leak.                                                                                                     | avutil §2.2                         |

## 7. swscale

| #   | Requirement (binding part)                                                                                                                                                                                                                                                                                                                                                                                                           | Rule                            |
| --- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------- |
| 7.1 | For grey, planar YUV 4:2:0 and 4:2:2, semi-planar and packed RGB output formats, the number of planes returned equals `av_pix_fmt_count_planes` and each plane's length equals `av_image_fill_plane_sizes`, for every output kind, on a first and on a second call. A paletted input is accepted with its palette buffer, and with empty buffers after it; a paletted output is accepted for frames and refused for the other kinds. | [swscale.md](swscale.md) §4.5   |
| 7.2 | Pixel content: a known image converted to another format and back equals the original within a stated tolerance; a solid colour stays that colour.                                                                                                                                                                                                                                                                                   | swscale §4.5                    |
| 7.3 | Several threads give byte for byte the output of one thread.                                                                                                                                                                                                                                                                                                                                                                         | swscale §4.5                    |
| 7.4 | Invalid input: a frame of another size or format, too few buffers, a short plane, a short line size, mismatched packed arrays: each raises.                                                                                                                                                                                                                                                                                          | swscale §4.4, §4.5; contract A1 |
| 7.5 | `scale` with a row offset writes the image at that row of the destination and nowhere else.                                                                                                                                                                                                                                                                                                                                          | swscale §4.4                    |
| 7.6 | A scaler creation with a rejected setting raises the FFmpeg error.                                                                                                                                                                                                                                                                                                                                                                   | swscale §4.3                    |

## 8. Environment

### 8.1 Versions

- The suite MUST run against the oldest and the newest supported FFmpeg
  release ([compatibility.md](compatibility.md) §1), with warnings as errors.
- It MUST run on the minimum and on the newest released OCaml version.
- It SHOULD run on Linux and macOS.

### 8.2 What the FFmpeg build must contain

The suite states the components it needs: encoders, decoders, muxers,
demuxers, filters, protocols, and the `ffmpeg` tool when fixtures are
synthesised with it. A missing component is reported per §10, never as a
pass.

The requirements above need at least: an audio codec with fixed frame size and
one with variable frame size; a video codec with frame reordering; an
intra-only image codec; a text subtitle codec; a container with a header and
one without (MPEG-PS); a bitstream filter with a private option; a filter with
an array option; a protocol that can block.

## 9. Build and cross-build

| #   | Requirement                                                                                                                                                                      | Rule                                            |
| --- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------- |
| 9.1 | With two FFmpeg versions installed and pkg-config pointed at one, every generated table matches that one's headers.                                                              | [build.md](build.md) §3.2                       |
| 9.2 | With the headers removed, or the preprocessor replaced by a failing program, the build fails and names the header.                                                               | build §3.2 G7                                   |
| 9.3 | Every member of each C enumeration the installed headers declare, markers excepted, converts to OCaml and back. Checked at build time for every table.                           | build §3.3; contract E2                         |
| 9.4 | Every hand-written constant is checked against the header at build time.                                                                                                         | contract E1                                     |
| 9.5 | With FFmpeg present and one library's stubs broken, the build fails. With FFmpeg absent, the build succeeds, reports every library unavailable, and the install target fails.    | build §2.5, §2.6                                |
| 9.6 | Installing FFmpeg and rebuilding, with no clean, enables the libraries. Upgrading it in place regenerates the tables.                                                            | build §2.1 D5                                   |
| 9.7 | A cross build for Windows from a machine whose own FFmpeg is another version: tables match the target's headers; every library links into an executable against a static FFmpeg. | [cross-compilation.md](cross-compilation.md) §2 |
| 9.8 | The stubs include nothing beyond what X12 allows.                                                                                                                                | cross-compilation X12                           |
| 9.9 | Both invocations of cross-compilation §3 succeed unchanged.                                                                                                                      | cross-compilation §3                            |

## 10. Harness

- **H1. The suite asserts that it ran.** The suite counts the requirements it
  checked and fails when the count is not the one expected for the
  environment. A run that executed nothing, or less than intended, is a
  failure. A build system that replays a cached result without executing the
  steps does not count as a run.
- **H2. A program asserts that it asserted.** A test program that made no
  check fails.
- **H3. Three outcomes.** A requirement is passed, failed or skipped. A skip
  names its reason (a missing component, a missing device) and is counted and
  printed. A skip is never reported as a pass, and the environment of §8
  fixes which skips are allowed.
- **H4. Seen to fail.** Each check of §12, and each regression check, is run
  once against a deliberately broken build and seen to fail before its pass
  counts. A sanitiser or leak detector is first shown to report a planted
  defect.
- **H5. Independence.** A failed requirement does not stop the others from
  running or being reported. A requirement does not read a file another
  requirement wrote.
- **H6. Time limit.** Every step has a time limit; exceeding it is a failure.
- **H7. A crash is a failure** that names the step: a death by signal is
  reported as such.
- **H8. Declared inputs.** The suite's result depends on the FFmpeg libraries
  it loaded: changing them re-runs it.
- **H9. The alias.** [build.md](build.md) §5.

## 11. Fixtures and doubles

Each fixture records the property that makes it trigger the condition it is
for. A convenient file that lacks the property passes for the wrong reason.

| Fixture                               | Defining property                                                                                                     |
| ------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| video with reordering                 | the decoder holds frames: B-frames, with a known frame count and an explicit frame rate                               |
| one-frame image                       | the only frame is still in the decoder when the demuxer ends                                                          |
| late streams                          | a headerless container whose extra streams are absent from the start of the file and from its end: probing reads both |
| audio, video and text subtitle in one | one stream of each kind, for selections and unhandled packets                                                         |
| text subtitles                        | several cues, multi-line cues, non-ASCII text, exact times                                                            |
| high bit rate stream                  | one write exceeds FFmpeg's I/O buffer                                                                                 |
| custom-order layout                   | the layout owns heap memory                                                                                           |

Doubles:

- read, write and seek closures over memory, with a mode that consumes part
  of each write and a mode that raises;
- an interrupt function that turns true after a delay;
- an endpoint that blocks: a listening socket nothing connects to;
- a log sink that records messages with the thread that logged them;
- a C helper that, given a container's native context through `av`'s
  installed header, calls its read and interrupt callbacks from a thread it
  creates;
- a second thread that allocates continuously, and a second thread that
  counts, for the lock checks of §12.

## 12. Known-complexity checks

Every entry of [known-complexity.md](known-complexity.md) is a check of the
suite: its "How to tell" paragraph is the check, by principle. The entry's
"Rule" line names the rule it verifies. Where a requirement above already
covers an entry, the entry names it.

Three run configurations serve many entries and are part of the suite:

- **Collection at every allocation.** Every operation that returns a compound
  value or takes a closure runs under a runtime that collects and compacts at
  each allocation, and its results are compared with a reference run
  (contract B1). A small minor heap is not this check.
- **Address and leak sanitisers.** The whole suite runs under both, with the
  runtime's own lifetime allocations suppressed and nothing else (H4).
- **Concurrent use.** For each handle with a guard, two threads, and two
  domains, call its operations at once: every call returns or raises the
  in-use error, and the sanitisers stay silent (contract M5, M6). An encoding
  loop and a decoding loop run at the same speed, within measurement noise,
  with the guards compiled out (contract M12).
