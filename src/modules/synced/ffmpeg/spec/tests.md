# Test suite of the OCaml FFmpeg bindings

This file describes the suite by principle. Each entry states what must
hold (Invariant), what a new suite must reproduce (Binding), what is an
accident of this one (Incidental), what it reads (Inputs), and where it
lives (Trace).

Three strengths of check appear below and the entries name which applies:

- **asserted** — the test compares a value and fails on a mismatch;
- **exercised** — the call runs; only an uncaught exception or a crash
  fails the test;
- **compiled** — the call is type-checked and linked, never run.

## 0. Shape of the suite

The suite is one build alias, `ffmpeg_citest`. It carries one rule whose
action is a fixed sequence of 45 commands, run in order in the test
directory, stopping at the first non-zero exit. The sequence contains:

- 11 dedicated test programs (`test_*`), 12 invocations;
- 21 example programs from the examples directory, 27 invocations, used
  both as smoke checks and as the producers of media for later steps;
- 4 invocations of the `ffmpeg` command-line tool that synthesise media;
- 1 line-ending normaliser and 1 `diff`.

Later steps read files written by earlier steps. The order is therefore
part of the suite. Section 9.3 gives the full order and the file each step
reads and writes.

The alias exists only when all seven binding libraries are detected as
available (section 8).

## 1. avutil

### 1.1 Colour property names round-trip

- **Invariant** — For a colour space, colour range, colour primaries,
  transfer characteristic and chroma location, converting a value to its
  FFmpeg name and looking that name up returns the same value.
- **Binding** — `from_name (name v) = Some v` for `Color_space` `` `Bt709``,
  `Color_range` `` `Mpeg``, `Color_primaries` `` `Bt709``, `Color_trc`
  `` `Bt709``, `Chroma_location` `` `Left``. One value per enumeration.
- **Incidental** — The choice of these five values; the message text.
- **Inputs** — None.
- **Trace** — `test_info.ml`.

### 1.2 Option getters read the native object

- **Invariant** — Reading a named AVOption from an opened input container
  returns the value held by the demuxer context.
- **Binding** — On an opened input, the 64-bit integer option `probesize`
  is strictly positive and the integer option `max_delay` is at least −1.
  Both getters return without error.
- **Incidental** — The two option names; the bounds (they are loose
  plausibility bounds, not the FFmpeg defaults).
- **Inputs** — Any openable media file; the suite passes the Matroska file
  of 3.7 and the FLAC stream of 2.5.
- **Trace** — `test_info.ml`.

### 1.3 Unused options are reported back, and only those

- **Invariant** — An option dictionary handed to an operation that opens a
  demuxer, a muxer or an encoder holds, after the call, exactly the keys
  FFmpeg did not consume. Consumed keys are removed. Keys survive intact
  whatever their number.
- **Binding** —
  1. Open an input with `probesize` = 5000000 plus two unknown keys: the
     dictionary afterwards holds exactly the two unknown keys.
  2. Open an output file with the two unknown keys only: both remain.
  3. Create an AAC audio encoder (stereo, 44100 Hz, planar float, time
     base 1/44100) with `b` = 128000 plus the two unknown keys: exactly the
     two unknown keys remain.
  4. Open an input with 20000 distinct unknown keys: all 20000 remain, and
     every remaining key is byte-identical to a key that was put in.
     The comparison is on the set of keys, not on values or order.
- **Incidental** — The key names (`definitely_not_an_option`, `also_bogus`,
  `bogus_option_NNNNNN`), the value `"1"`, the count 20000, the output file
  name, the final full collection.
- **Inputs** — The Matroska file of 3.7. The output file is created and
  closed with no stream.
- **Trace** — `test_options.ml`.

The same rule is asserted for two more entry points by an example: see
3.9.

### 1.4 Standard channel layouts enumerate and describe

- **Invariant** — The list of standard channel layouts can be built and
  each layout has a textual description.
- **Binding** — Exercised only: enumeration and description of every
  element complete without error.
- **Incidental** — The printed list.
- **Inputs** — None.
- **Trace** — `examples/all_channel_layouts.ml`.

### 1.5 Log redirection

- **Invariant** — With the log level at debug and a user callback
  installed, every FFmpeg log line produced during demuxing, decoding,
  encoding and muxing reaches the callback as a string, from whichever
  thread FFmpeg logs on, without crashing the process.
- **Binding** — Exercised only. Most programs in the suite install a
  callback that prints, at debug level; two install a callback that
  discards. No log content is asserted.
- **Incidental** — Everything printed.
- **Inputs** — Whatever the host program reads.
- **Trace** — `test_info.ml`, `test_resample.ml`, most examples.

## 2. avcodec

### 2.1 Encoder capabilities are typed values

- **Invariant** — The capability list of an encoder is non-empty and holds
  values of the public capability enumeration.
- **Binding** — The AAC encoder, found by codec id, reports at least one
  capability and the list contains `` `Dr1``.
- **Incidental** — The choice of AAC and of `` `Dr1``. The code comment
  gives the reason for `` `Dr1``: every encoder FFmpeg ships sets
  `AV_CODEC_CAP_DR1`.
- **Inputs** — None. FFmpeg must be built with the native AAC encoder.
- **Trace** — `test_codec.ml`.

### 2.2 Codec id survives a lookup

- **Invariant** — The id reported by a decoder found by id is the id that
  was asked for, for each media kind.
- **Binding** — For audio `` `Aac``, video `` `H264`` and subtitle
  `` `Subrip``: the name of the id of the decoder found for that id equals
  the name of that id. The comparison is on names.
- **Incidental** — The three ids.
- **Inputs** — None. FFmpeg must have AAC, H.264 and SubRip decoders.
- **Trace** — `test_codec.ml`.

### 2.3 Codec and bitstream-filter catalogues enumerate

- **Invariant** — Every audio, video and subtitle codec id has a name and
  an optional descriptor (media type, names, MIME types, properties,
  profiles). Every encoder and decoder has a name and a description. Every
  video codec reports its supported pixel formats and colour spaces. Every
  bitstream filter reports its name, codec ids and option table.
- **Binding** — Exercised only: the full walk completes.
- **Incidental** — All printed text.
- **Inputs** — None.
- **Trace** — `examples/all_codecs.ml`, `examples/all_bitstream_filters.ml`.

### 2.4 Stand-alone audio encoding

- **Invariant** — An encoder created without a container accepts frames
  and delivers packets, then delivers its remaining packets on flush.
- **Binding** — Exercised here; asserted downstream: the FLAC output is
  later demuxed and decoded to a non-zero number of samples (5.4) and
  probed to a stream with a positive sample rate and channel count (3.1).
  The encoder is fed 2001 frames. The frame length is 512 samples when the
  encoder accepts variable frame sizes, otherwise the encoder's own frame
  size. Each frame is cut from a buffer twice that long with an offset of
  10 samples (5.5).
- **Incidental** — 440 Hz sine, mono source up-mixed to stereo at 44100 Hz,
  the sample format picked as the encoder's closest to double.
- **Inputs** — Generated in process. Run twice: `flac` and `mp2`. The
  output is the concatenation of packet payloads with no container.
- **Trace** — `examples/encode_audio.ml`.

### 2.5 Stand-alone decoding of demuxed packets

- **Invariant** — Decoders created from stream parameters accept the
  packets the demuxer returns for that stream and flush cleanly.
- **Binding** — Exercised only. Every packet returned is audio or video;
  any other result fails. Decoded frames are discarded.
- **Incidental** — Nothing else.
- **Inputs** — The Matroska file of 3.7.
- **Trace** — `examples/decoding.ml`.

### 2.6 Hardware encoding

- **Invariant** — None enforced. The program looks up `h264_nvenc`, lists
  its hardware configurations and, when a device or frame context can be
  created, encodes 241 frames.
- **Binding** — None: every outcome short of a crash exits with status 0.
- **Incidental** — Everything.
- **Inputs** — An NVENC-capable host, which the reference CI does not
  have.
- **Trace** — `examples/hw_encode.ml`, run with modes `device` and `frame`.

## 3. avformat

### 3.1 Probing an input

- **Invariant** — An opened input exposes at least one stream. Audio
  streams have a positive sample rate and channel count. Video streams have
  positive width and height and yield at least one decoded frame.
- **Binding** — Those four assertions, for each file given. For each video
  stream a decoder is also created from the stream parameters (exercised).
  Duration, container and stream metadata, container-level stream time
  base, codec id, sample format, bit rate, sample aspect ratio and the
  colour properties of the first video frame are read (exercised).
- **Incidental** — The printed report, including its unit labels.
- **Inputs** — The Matroska file of 3.7 and the FLAC stream of 2.4.
- **Trace** — `test_info.ml`.

### 3.2 Reading subtitle frames

- **Invariant** — A container with a text subtitle stream yields decoded
  subtitle frames when that stream is selected for frame output, then end
  of file.
- **Binding** — The input has at least one subtitle stream; at least one
  subtitle frame is read; reading ends with the end-of-file error; any
  other error fails. Content (timestamp, display time, rectangles) is read
  but not compared.
- **Incidental** — Printed lines.
- **Inputs** — The subtitle Matroska file of 10.2.
- **Trace** — `test_subtitle_read.ml`.

### 3.3 Packets of unselected streams go to the unhandled-packet callback

- **Invariant** — When reading with only some streams selected, packets of
  the other streams are handed, typed by media kind, to the caller's
  callback rather than dropped or returned.
- **Binding** — Selecting all audio streams for frame output: at least one
  audio frame is returned, and the callback receives at least one video
  packet when the input has a video stream. Reading ends with end of file;
  any other error fails.
- **Incidental** — The printed counters. The count of unhandled subtitle
  packets is printed and not compared.
- **Inputs** — The subtitle Matroska file of 10.2 (audio, video, subtitle).
- **Trace** — `test_unhandled_packet.ml`.

### 3.4 A seek discards frames buffered before it

- **Invariant** — After a seek, the first frame returned for a stream
  belongs to the new position. Frames a decoder was holding from before
  the seek are not returned.
- **Binding** — Read 10 video frames, seek the container to 20000 ms with
  default flags and no stream, read one video frame: it has a timestamp,
  and that timestamp, converted with the stream time base, is at least
  19 s.
- **Incidental** — The 10 warm-up reads, the 1 s tolerance, the target.
- **Inputs** — The B-frame Matroska file of 10.2. B-frames are what make the
  decoder hold frames.
- **Trace** — `test_seek.ml`.

### 3.5 Decoders are drained at end of input

- **Invariant** — Every frame of a stream is returned before the
  end-of-file error, including frames the decoder still holds when the
  demuxer runs out of packets. After end of file a seek to the start makes
  the whole stream readable again with the same frame count.
- **Binding** —
  1. The B-frame video (25 s at 25 fps) yields exactly 625 video frames.
  2. After seeking that input to 0 ms it yields exactly 625 again.
  3. A one-frame PNG yields exactly 1 video frame.
- **Incidental** — Program arguments; the third argument is tested only for
  presence.
- **Inputs** — The B-frame Matroska file and the PNG of 10.2.
- **Trace** — `test_drain.ml`.

### 3.6 Streams found while reading are tracked

- **Invariant** — For a demuxer that discovers streams during reading, the
  input's stream list grows accordingly, and closing the input afterwards
  is safe.
- **Binding** — The number of audio streams after reading the best audio
  stream to end of file is strictly greater than the number right after
  opening. Closing then returns normally.
- **Incidental** — The exact counts.
- **Inputs** — The MPEG program stream of 10.2. It depends on the MPEG-PS
  demuxer not finding the late streams during probing.
- **Trace** — `test_input_streams_grow.ml`.

### 3.7 Muxing encoded audio and video to a file

- **Invariant** — An output container accepts an audio and a video stream
  with encoders, container and stream metadata, and frames with
  caller-set timestamps; closing it writes a playable file.
- **Binding** — Exercised here; asserted downstream by 1.2, 1.3, 3.1, 2.5,
  3.8, 3.10, 3.11, 4.2, 4.3, 5.4, 6.2, which all read the result.
- **Incidental** — Container title `On Off`; stream metadata `Media`; the
  on/off pattern; the subtitle codec argument, which is ignored.
- **Inputs** — Generated in process: 250 video frames 352×288 `yuv420p` at
  25 fps (MPEG-4), and 250 audio frames of the encoder's frame size, stereo
  44100 Hz (AAC), 440 Hz sine alternating with silence. Written as
  `out.mkv`.
- **Trace** — `examples/encoding.ml`.

A second program encodes 241 frames 352×288 in `yuva420p` with
`libvpx-vp9` to WebM. Exercised only; nothing reads the result. It also
reads the pixel format descriptor. Trace: `examples/encode_video.ml`.

### 3.8 Remuxing packets, with packet side data

- **Invariant** — Packets read in packet mode from every audio, video and
  subtitle stream can be written unchanged to streams created as copies of
  the input parameters. Side data added to a packet can be read back from
  it.
- **Binding** — Exercised. Every result is an audio, video or subtitle
  packet; anything else fails. Three side-data items (string metadata,
  metadata update, replay gain 1/2/3/4) are added to each audio packet and
  the list is read back and printed, not compared. The video stream's
  average frame rate is copied.
- **Incidental** — Printed side data.
- **Inputs** — `out.mkv` of 3.7; output MP4.
- **Trace** — `examples/remuxing.ml`.

### 3.9 Custom I/O output, and option reporting on stream creation

- **Invariant** — An output can be driven through caller write and seek
  callbacks with a guessed output format. Options given when opening the
  output and when creating an encoded stream follow the rule of 1.3.
- **Binding** — Asserted:
  - opening with `packetsize` = 4096 and `foo`: `packetsize` is removed,
    `foo` remains;
  - creating the audio stream with `lpc_type` = `none` and `foo`: `foo`
    remains; `lpc_type` is removed for FLAC and remains for MP2.
    Exercised: 2001 frames through a resampler and the audio frame-size
    converter of 4.4, then flush and close.
- **Incidental** — Output sample rate 22050 for FLAC and 44100 otherwise.
- **Inputs** — Generated in process. Run for `flac` and `mp2`.
- **Trace** — `examples/encode_stream.ml`.

### 3.10 Custom I/O input

- **Invariant** — An input can be driven through caller read and seek
  callbacks with no format hint, and decodes like a file input.
- **Binding** — Exercised. Each result is an audio frame of the best
  stream; anything else fails. An invalid-data error is skipped; end of
  file ends the loop.
- **Incidental** — The printed format line.
- **Inputs** — The FLAC and MP2 streams of 2.4; re-encoded with
  `libmp3lame` to MP3 and `libvorbis` to Ogg.
- **Trace** — `examples/decode_stream.ml`.

### 3.11 Transcoding frames, including subtitles

- **Invariant** — Frames decoded from every stream can be encoded into new
  streams of a second container. Frame metadata can be read and replaced.
- **Binding** — Exercised. Every result is an audio, video or subtitle
  frame.
- **Incidental** — The `encoder` metadata entry.
- **Inputs** — `out.mkv` of 3.7 (no subtitle stream, so the subtitle branch
  does not run); output MP4.
- **Trace** — `examples/transcoding.ml`.

### 3.12 Reading metadata

- **Invariant** — Container metadata and per-stream metadata of audio and
  video streams are readable as key/value lists.
- **Binding** — Exercised only; only the first program argument is used.
- **Incidental** — Printed lines; the three unused arguments.
- **Inputs** — The FLAC and MP2 streams of 2.4.
- **Trace** — `examples/read_metadata.ml`.

### 3.13 Subtitle content survives decode, rebuild, encode, mux

- **Invariant** — Text subtitles read as frames, taken apart into their
  content (timing, rectangles, text, ASS line), rebuilt into new frames
  from that content, encoded as SubRip and muxed, give the same subtitle
  text and timing as the original.
- **Binding** — Asserted byte for byte: the SubRip file produced, with
  every carriage return removed, equals the fixture. This covers cue
  numbering, start and end times to the millisecond, multi-line cues,
  accented Latin and Japanese text, and cue order.
- **Incidental** — The intermediate file names; the progress lines.
- **Inputs** — The subtitle Matroska file of 10.2, itself built from the
  fixture.
- **Trace** — `examples/subtitle_remux.ml`, `normalize_line_endings.ml`,
  `fixtures/sample.srt`.

### 3.14 Blocking opens can be interrupted

- **Invariant** — An open that blocks returns with the `` `Exit`` error
  once the caller's interrupt callback returns true.
- **Binding** — Asserted, outside GitHub Actions only: opening an input on
  a listening Unix socket, and opening an output on
  `http://localhost/foo.mp3`, both fail with `` `Exit`` after the callback
  turns true (about 0.1 s after start). Any other outcome fails.
- **Incidental** — The 0.1 s delay, the socket path, the URL.
- **Inputs** — A temporary file name used as a Unix socket path. The
  protocols `unix` and `http` must be present.
- **Trace** — `examples/interrupt.ml`.

## 4. avfilter

### 4.1 Filter catalogue

- **Invariant** — The four buffer and sink filters and every registered
  filter expose name, description, flags, option table and typed input and
  output pads.
- **Binding** — Exercised only.
- **Incidental** — The printed catalogue.
- **Inputs** — None.
- **Trace** — `examples/list_filters.ml`.

### 4.2 Video graph built by hand

- **Invariant** — A graph `buffer → fps → buffersink`, attached and linked
  pad by pad and launched, accepts frames on its input, returns frames from
  its sink until it reports "try again", and reports end of file after a
  flush.
- **Binding** — Exercised. The sink's time base, frame rate, width, height
  and pixel aspect are read after launch.
- **Incidental** — The printed sink description.
- **Inputs** — `out.mkv` of 3.7; output MP4 (AAC, MPEG-4).
- **Trace** — `examples/fps.ml`.

### 4.3 Audio graph built by hand

- **Invariant** — A graph `abuffer → aresample → aformat → abuffersink`
  works the same way; list-valued options are passed as arrays when the
  filter declares an array separator and as a `|`-joined string otherwise;
  the sink's frame size can be fixed after launch.
- **Binding** — Exercised. The sink's time base, channel count, layout,
  sample rate and format are read and used to create the output stream.
- **Incidental** — Target rate 22050; formats `s16`, `fltp`; layouts
  `stereo`, `mono`.
- **Inputs** — `out.mkv` of 3.7; output MP4.
- **Trace** — `examples/aresample.ml`.

### 4.4 Audio frame-size converter

- **Invariant** — The audio converter helper re-frames (and optionally
  reformats) audio to a fixed frame size and delivers the remainder on
  flush.
- **Binding** — Exercised by three programs; every frame it delivers is
  written to an encoder that requires that frame size.
- **Incidental** — Frame size 512 for variable-size encoders.
- **Inputs** — As 3.9, 3.10; also an Ogg-to-AAC transcode.
- **Trace** — `examples/encode_stream.ml`, `examples/decode_stream.ml`,
  `examples/transcode_aac.ml`.

## 5. swresample

### 5.1 Every output container kind carries the right samples

- **Invariant** — Converting mono 44100 Hz to mono 44100 Hz in double
  precision is the identity, for every kind of output container.
- **Binding** — A 1024-sample ramp from −1 to just under 1 is converted
  from a float array. For each output kind — float array, planar float
  array, interleaved bytes, planar bytes, interleaved bigarray, planar
  bigarray, and frame (read back through a frame-to-float-array
  converter) — the result has exactly 1024 samples and each sample is
  within 0.01 of the ramp. Planar results have exactly one plane. Bytes
  hold little-endian IEEE doubles.
  The planar frame output is exercised only.
- **Incidental** — The ramp, 1024, the 0.01 tolerance.
- **Inputs** — Generated in process.
- **Trace** — `test_swresample.ml`.

### 5.2 Output length follows the converted count

- **Invariant** — When the output holds fewer samples than the upper bound
  it was allocated for, its reported length is the converted count.
- **Binding** — Converting the same ramp from 44100 to 22050 Hz, the
  interleaved bigarray, the planar bigarray (one plane) and the frame each
  report a length within 16 of 512.
- **Incidental** — The tolerance 16.
- **Inputs** — Generated in process.
- **Trace** — `test_swresample.ml`.

### 5.3 Converters chain across formats, layouts and rates

- **Invariant** — The output container of one converter is a valid input
  of the next, across interleaved and planar forms, integer and float
  formats, and mono, stereo, 5.1 and down-mix layouts.
- **Binding** — For 96 sine notes, a direct conversion (mono float array
  to stereo signed-32 bytes) yields a non-zero total byte count, and a
  chain of eleven converters yields a non-zero total byte count. The
  chain's stages, in order: float array → S16 frame (mono→5.1,
  44100→96000) → U8 bigarray (→stereo 16000) → double planar frame
  (→44100) → S32 planar bigarray (→48000) → float planar bigarray
  (→down-mix 31000) → planar float array (→stereo 73347) → S32 frame
  (→44100) → float array (→48000) → S32 bigarray (→96000) → S32 bytes
  (→44100) → S32 bytes (→mono).
- **Incidental** — The two raw output files; nothing reads them. Note
  frequencies and lengths.
- **Inputs** — Generated in process.
- **Trace** — `test_resample.ml`.

### 5.4 Resampling decoded frames from codec parameters

- **Invariant** — A converter configured from a stream's codec parameters
  accepts that stream's decoded frames.
- **Binding** — For each input file, converting every frame of the best
  audio stream to stereo 44100 Hz planar floats yields a non-zero total
  sample count. At least one input file must be given.
- **Incidental** — The raw S16LE file written per input.
- **Inputs** — The FLAC stream of 2.4.
- **Trace** — `test_resample.ml`.

### 5.5 Other exercised paths

Exercised only: conversion of a sub-range (`offset` 10, explicit length)
of a float array to a frame; a converter configured towards codec
parameters; frame to S32 bytes with the SoX resampler engine, on Ogg and
Matroska inputs opened with an explicitly named input format. Trace:
`examples/encode_audio.ml`, `examples/encoding.ml`,
`examples/audio_decoding.ml`, `examples/demuxing_decoding.ml`.

## 6. swscale

### 6.1 Output planes are sized by the pixel format

- **Invariant** — On the byte-string output path each plane has the size
  the pixel format gives it, chroma subsampling included, on every call.
- **Binding** — Converting a 64×64 `rgb24` image (one plane, stride 192)
  to 64×64 `yuv420p` with the bilinear flag gives 3 planes of 4096, 1024
  and 1024 bytes. The same holds on a second call on the same context.
- **Incidental** — The fill byte `0x80`; the image size.
- **Inputs** — Generated in process.
- **Trace** — `test_swscale.ml`.

### 6.2 Scaling decoded frames

Exercised only: each decoded video frame of `out.mkv` is scaled from
352×288 to 800×600 `yuv420p`, frame in, bigarray planes out, with an empty
flag list. The result is discarded. Trace: `examples/demuxing_decoding.ml`.

## 7. avdevice

No program in the suite calls the device library. Three example programs
that use it are compiled and linked with the rest and never run.

## 8. build

### 8.1 All or nothing

- **Invariant** — The test programs and the examples are defined only when
  all seven libraries (`avutil`, `avcodec`, `avfilter`, `av`, `swscale`,
  `swresample`, `avdevice`) are detected.
- **Binding** — The gate is a single boolean: the conjunction of the seven
  per-library availability files, each of which must read exactly `true`
  on its first line. When it is false the generators emit nothing: no test
  executable, no example executable, no `ffmpeg_citest` alias.
- **Incidental** — The generator programs and the include-file mechanism.
- **Trace** — `gen/gen_test.ml`, `gen/dune`, `test/dune`.

### 8.2 Everything links

- **Invariant** — Each of the 11 test programs builds against the
  `av`, `swresample` and `swscale` libraries; each of the 28 examples
  builds against the libraries it names.
- **Binding** — Compiled. This is the only check on the device library and
  on the 7 examples that are never run.
- **Trace** — `gen/gen_test.ml`, `gen/gen_examples.ml`.

### 8.3 Generated enumeration tables produce real variants

- **Invariant** — Values produced from the generated C-to-OCaml tables are
  usable as the public polymorphic variants.
- **Binding** — Covered by 2.1 (a literal `` `Dr1`` is found in a list
  that came from C) and 1.1.
- **Trace** — `test_codec.ml`, `test_info.ml`.

## 9. Harness

### 9.1 Runner

The runner is a separate program started once per step:
`runner <label> <program> [args…]`.

1. It prints a header naming the step: a `::group::<label>` line when the
   environment variable `GITHUB_ACTIONS` is set, a banner otherwise.
2. It starts `./<program>` as a child process with the given arguments,
   inheriting standard input, output and error. The working directory is
   the test directory of the build tree.
3. It waits for the child with no time limit.
4. Exit status: a normal exit gives the child's code; a child killed by a
   signal gives 128 plus the signal number as the OCaml runtime reports
   it; a stopped child gives 128.
5. On 0 it prints `PASSED: <label>`. Otherwise it prints
   `FAILED: <label> (exit code N)` and, under GitHub Actions,
   `::error::Test <label> failed`.
6. It prints a footer (`::endgroup::` or a rule) and exits with the same
   status.

With fewer than two arguments it prints a usage line and exits 1.

Three steps bypass the runner and are run directly by the build rule: the
subtitle remux, the normaliser and `diff`. So are the four `ffmpeg`
commands.

The build rule runs the steps as a strict sequence. The first non-zero
status ends the rule; later steps do not run.

**Binding for a new harness**: each step is a separate process; a non-zero
exit or a death by signal fails the suite; the order of 9.3. **Incidental**:
the label, banner and group syntax, the `128 + n` encoding, the absence of
a timeout, running everything in one rule.

### 9.2 Assertion helper

A small module shared by the 11 test programs keeps two counters, checks
made and checks failed.

- `check name ok` counts one check; prints `OK: <name>` on standard output
  when it holds, `FAILED: <name>` on standard error when it does not.
  Execution continues after a failure.
- `checkf ok fmt …` is the same with a formatted name.
- `finish ()` prints `<n> checked, <m> failed`, then exits 1 when no check
  was made (message `FAILED: test asserted nothing`) or when any failed.
  Otherwise it returns and the program exits 0.

Ten of the 11 test programs end with `finish ()`. The unhandled-packet
test does not use the helper: it exits 1 itself on its two conditions.

An uncaught exception ends a program with the OCaml runtime's status 2.

**Binding**: a test that makes no check fails; a failed check fails the
test; checks after a failed one still run. **Incidental**: the wording and
the stream each line goes to.

### 9.3 Order, arguments and files

`→` marks a file written, `←` a file read. All paths are relative to the
test directory of the build tree.

| #   | Label                 | Program and arguments                                           | Files                            |
| --- | --------------------- | --------------------------------------------------------------- | -------------------------------- |
| 1   | codec                 | `test_codec`                                                    |                                  |
| 2   | swscale               | `test_swscale`                                                  |                                  |
| 3   | swresample            | `test_swresample`                                               |                                  |
| 4   | list_filters          | `list_filters`                                                  |                                  |
| 5   | all_codecs            | `all_codecs`                                                    |                                  |
| 6   | all_channel_layouts   | `all_channel_layouts`                                           |                                  |
| 7   | all_bitstream_filters | `all_bitstream_filters`                                         |                                  |
| 8   | hw_encode_device      | `hw_encode nvenc.mp4 h264_nvenc device`                         | → nvenc.mp4                      |
| 9   | hw_encode_frame       | `hw_encode nvenc.mp4 h264_nvenc frame`                          | → nvenc.mp4                      |
| 10  | interrupt             | `interrupt`                                                     |                                  |
| 11  | encode_audio_flac     | `encode_audio A4.flac flac`                                     | → A4.flac                        |
| 12  | encode_audio_mp2      | `encode_audio A4.mp2 mp2`                                       | → A4.mp2                         |
| 13  | encode_video_webm     | `encode_video video.webm yuva420p libvpx-vp9`                   | → video.webm                     |
| 14  | encode_stream_flac    | `encode_stream S4.flac flac`                                    | → S4.flac                        |
| 15  | encode_stream_mp2     | `encode_stream S4.mp2 mp2`                                      | → S4.mp2                         |
| 16  | encoding              | `encoding out.mkv aac mpeg4 ass`                                | → out.mkv                        |
| 17  | resample              | `test_resample A4.flac`                                         | ← A4.flac → three `.raw`         |
| 18  | info                  | `test_info out.mkv A4.flac`                                     | ← both                           |
| 19  | options               | `test_options out.mkv`                                          | ← out.mkv → test_options_out.mkv |
| 20  | decode_stream_mp3     | `decode_stream A4.flac A4.mp3 libmp3lame`                       | ← A4.flac → A4.mp3               |
| 21  | decode_stream_ogg     | `decode_stream A4.mp2 A4.ogg libvorbis`                         | ← A4.mp2 → A4.ogg                |
| 22  | audio_decoding_ogg    | `audio_decoding A4.ogg ogg A4`                                  | ← A4.ogg → A4.raw                |
| 23  | audio_decoding_mkv    | `audio_decoding out.mkv matroska out`                           | ← out.mkv → out.raw              |
| 24  | remuxing              | `remuxing out.mkv out_remuxed.mp4`                              | ← out.mkv                        |
| 25  | transcode_aac         | `transcode_aac A4.ogg A4_transcoded.mp4`                        | ← A4.ogg                         |
| 26  | transcoding           | `transcoding out.mkv out_transcoded.mp4`                        | ← out.mkv                        |
| 27  | decoding              | `decoding out.mkv`                                              | ← out.mkv                        |
| 28  | fps                   | `fps out.mkv out_fps.mp4`                                       | ← out.mkv                        |
| 29  | aresample             | `aresample out.mkv out_aresample.mp4`                           | ← out.mkv                        |
| 30  | demuxing_decoding     | `demuxing_decoding out.mkv vo.raw ao.raw`                       | ← out.mkv                        |
| 31  | read_metadata_flac    | `read_metadata A4.flac flac A4.mp3 libmp3lame`                  | ← A4.flac                        |
| 32  | read_metadata_mp2     | `read_metadata A4.mp2 mp2 A4.ogg libvorbis`                     | ← A4.mp2                         |
| 33  | —                     | `ffmpeg` (subtitle file, 10.2)                                  | ← fixture → test_with_subs.mkv   |
| 34  | subtitle_read         | `test_subtitle_read test_with_subs.mkv`                         |                                  |
| 35  | unhandled_packet      | `test_unhandled_packet test_with_subs.mkv`                      |                                  |
| 36  | —                     | `ffmpeg` (B-frame file, 10.2)                                   | → test_seek.mkv                  |
| 37  | seek                  | `test_seek test_seek.mkv`                                       |                                  |
| 38  | drain_video           | `test_drain test_seek.mkv 625 reseek`                           |                                  |
| 39  | —                     | `ffmpeg` (PNG, 10.2)                                            | → test_drain.png                 |
| 40  | drain_image           | `test_drain test_drain.png 1`                                   |                                  |
| 41  | —                     | `ffmpeg` (MPEG-PS, 10.2)                                        | → test_input_streams_grow.mpg    |
| 42  | input_streams_grow    | `test_input_streams_grow test_input_streams_grow.mpg`           |                                  |
| 43  | —                     | `subtitle_remux test_with_subs.mkv raw_remuxed_subs.srt subrip` | → raw_remuxed_subs.srt           |
| 44  | —                     | normaliser `raw_remuxed_subs.srt remuxed_subs.srt`              | → remuxed_subs.srt               |
| 45  | —                     | `diff fixtures/sample.srt remuxed_subs.srt`                     |                                  |

None of the written files is a declared build target. They stay in the
build tree between runs.

### 9.4 Skips

The harness has no notion of a skipped test. Two programs skip by exiting
0, which the runner reports as `PASSED`:

- the interrupt example exits 0 at once when `GITHUB_ACTIONS` is set;
- the hardware-encode example exits 0 when the encoder is absent and
  catches every exception of its optional path.

### 9.5 Output comparison

There is one comparison against an expected file: step 45. `diff` is given
the fixture and the normalised output; a difference is a non-zero status.
There is no snapshot or promotion mechanism. All other output is printed
and discarded.

### 9.6 Line-ending normalisation

The normaliser reads its first argument as bytes, removes every carriage
return (`0x0D`) wherever it occurs, and writes the result to its second
argument. The output file is opened in text mode.

It runs once, on the SubRip file produced by step 43, before the `diff`.
The fixture uses bare line feeds. The code does not record why the
produced file can contain carriage returns (see findings).

**Binding**: the comparison of 3.13 ignores carriage returns in the
produced file. **Incidental**: doing it with a separate program.

### 9.7 Reference CI

The bindings' own workflow builds FFmpeg from source (tags `n8.0.1` and
`n7.1`) with `libtheora`, `libvpx`, `libmp3lame`, `libvorbis` and
`libsoxr`, on Linux and macOS with OCaml 4.14 and 5.3, then runs
`dune build @ffmpeg_citest`.

## 10. Doubles and fixtures a new harness needs

### 10.1 Fixture file

One checked-in file: a UTF-8 SubRip file, bare line feeds, 5 cues, with a
blank line after the last:

| Cue | Start        | End          | Lines                        |
| --- | ------------ | ------------ | ---------------------------- |
| 1   | 00:00:01,000 | 00:00:04,000 | 1 line, ASCII                |
| 2   | 00:00:05,000 | 00:00:08,000 | 2 lines, ASCII               |
| 3   | 00:00:09,500 | 00:00:12,000 | 2 lines, with `épiphénomène` |
| 4   | 00:00:13,000 | 00:00:16,500 | 2 lines, with `こんにちは`   |
| 5   | 00:00:17,000 | 00:00:20,000 | 2 lines, ASCII               |

### 10.2 Media synthesised with the `ffmpeg` tool

All four use `-y` and `lavfi` sources.

| File                          | Recipe                                                                                                                                                                                                                                 | Property the tests rely on                                                                           |
| ----------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| `test_with_subs.mkv`          | `color=c=blue:s=320x240:d=25` as MPEG-4; `anullsrc=r=44100:cl=stereo:d=25` as AAC; the fixture as `srt`; `-shortest`                                                                                                                   | one video, one audio, one text subtitle stream                                                       |
| `test_seek.mkv`               | `color=c=blue:s=320x240:d=25` as MPEG-4 with `-bf 2`                                                                                                                                                                                   | 625 frames; B-frames, so the decoder buffers                                                         |
| `test_drain.png`              | `color=c=blue:s=144x144`, `-frames:v 1`                                                                                                                                                                                                | exactly one frame, held by the decoder at end of input                                               |
| `test_input_streams_grow.mpg` | input 0: `sine=f=440:d=12:r=48000`; input 1: `sine=f=880:d=2:r=16000` with `-itsoffset 6`; maps `0:a` once and `1:a` four times; all audio MP2 mono 32 kb/s, except stream 0 as `pcm_s16be` stereo; `-muxrate 20000000`; format `mpeg` | four streams start 6 s in and last 2 s: after the probe window and before the tail read for duration |

### 10.3 Media produced by the bindings themselves

| File                | Producer | Content                                                |
| ------------------- | -------- | ------------------------------------------------------ |
| `A4.flac`, `A4.mp2` | 2.4      | raw encoder packets, 440 Hz, stereo 44100 Hz           |
| `out.mkv`           | 3.7      | MPEG-4 352×288 25 fps, 250 frames; AAC stereo 44100 Hz |
| `A4.ogg`, `A4.mp3`  | 3.10     | re-encodes of the two above                            |

A new harness may synthesise these with the `ffmpeg` tool instead. Doing
so removes the dependency of the read-side tests on the write side, and
removes the only downstream check on the write side.

### 10.4 Doubles

- Caller-supplied read, write and seek functions over a file descriptor,
  for custom I/O (3.9, 3.10).
- An interrupt function that turns true after a delay, set from a second
  thread (3.14).
- A Unix-socket path that nothing connects to, and a local HTTP URL with
  no server, as blocking endpoints (3.14).
- A log sink (1.5).
- An unhandled-packet counter per media kind (3.3).

### 10.5 What the FFmpeg build must contain

- Tool: `ffmpeg` on the path, with `lavfi` and the sources `color`,
  `anullsrc`, `sine`.
- Encoders: `aac`, `mpeg4`, `flac`, `mp2`, `pcm_s16be`, `png`, `srt`/
  `subrip`, `libvpx-vp9`, `libmp3lame`, `libvorbis`.
- Decoders: the matching ones, plus `h264` (looked up only).
- Muxers and demuxers: Matroska, WebM, MP4, FLAC, MP2, MP3, Ogg, MPEG-PS,
  SubRip, PNG image.
- Filters: `fps`, `aresample`, `aformat`, the four buffer filters.
- Protocols: `file`, `unix`, `http`.
- libswresample built with the SoX resampler.

A missing component fails the step that needs it. Only `h264_nvenc` is
optional.

## 11. Coverage map

Legend: **A** asserted, **E** exercised, **C** compiled only, **—** not
touched. The number is the entry above.

### avutil

| Area                                                                                                                                                                        | Coverage                              |
| --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------- |
| Version (`version`, `version_string`, `compare_version`)                                                                                                                    | —                                     |
| `Frame`: `pts`                                                                                                                                                              | A 3.4 (present after seek)            |
| `Frame`: `set_pts`, `metadata`, `set_metadata`                                                                                                                              | E 3.7, 3.11                           |
| `Frame`: `duration`, `set_duration`, `pkt_dts`, `set_pkt_dts`, `best_effort_timestamp`, `copy`                                                                              | —                                     |
| Errors: `` `Eof`` as end of input                                                                                                                                           | A 3.2, 3.5 and every read loop        |
| Errors: `` `Exit``                                                                                                                                                          | A 3.14 (not on CI)                    |
| Errors: `` `Eagain``, `` `Invalid_data``                                                                                                                                    | E 4.2, 4.3, 3.10                      |
| Errors: every other constructor, `string_of_error` content                                                                                                                  | —                                     |
| `expr_parse_and_eval`, `create_data`, `string_of_rational`, `qp2lambda`                                                                                                     | —                                     |
| `Time_format`: `` `Millisecond``                                                                                                                                            | A 3.4, 3.5; others —                  |
| `time_base ()`                                                                                                                                                              | E 3.2, 3.13                           |
| `Log`: `set_level`, `set_callback`                                                                                                                                          | E 1.5                                 |
| `Log`: `clear_callback`, level filtering                                                                                                                                    | —                                     |
| `Channel_layout`: `mono`, `stereo`, `five_point_one`, `find`, `standard_layouts`, `get_description`                                                                         | E 5.1, 5.3, 1.4                       |
| `Channel_layout`: `compare`, `get_nb_channels`, `get_default`, `get_mask`, `get_native_id`, failed `find`                                                                   | —                                     |
| `Sample_format`: `get_name`, `get_id`                                                                                                                                       | E 3.1, 4.3                            |
| `Sample_format`: `find`, `find_id`                                                                                                                                          | —                                     |
| Colour enumerations `name` / `from_name`                                                                                                                                    | A 1.1, one value each; unknown name — |
| `Pixel_format`: `descriptor`, `bits`, `of_string`, `to_string`, `get_id`                                                                                                    | E 3.7, 2.3, 4.2                       |
| `Pixel_format`: `planes`, `find_id`, failed `of_string`                                                                                                                     | —                                     |
| `Audio`: `frame_nb_samples`                                                                                                                                                 | A 5.2                                 |
| `Audio`: `frame_get_sample_format`, `_sample_rate`, `_channel_layout`                                                                                                       | E 3.10                                |
| `Audio`: `create_frame`, `frame_get_channels`, `frame_copy_samples`                                                                                                         | —                                     |
| `Video`: `create_frame`, `frame_visit`                                                                                                                                      | E 3.7                                 |
| `Video`: colour getters on a frame                                                                                                                                          | E 3.1                                 |
| `Video`: `frame_get_linesize`, `_width`, `_height`, `_pixel_format`, `_pixel_aspect`                                                                                        | —                                     |
| `Subtitle`: `get_content`, `create_frame` (text)                                                                                                                            | A 3.13                                |
| `Subtitle`: bitmap rectangles, `header_ass_default`, `get_pts`                                                                                                              | — (bitmap: C)                         |
| `Options`: `get_int`, `get_int64`                                                                                                                                           | A 1.2 (loose)                         |
| `Options`: `opts` (option tables)                                                                                                                                           | E 2.3, 4.1                            |
| `Options`: the nine other getters, `search_children`, missing name                                                                                                          | —                                     |
| Option dictionaries: unused-key reporting                                                                                                                                   | A 1.3, 3.9                            |
| Option dictionaries: `opts_default`, `mk_opts_array`, `string_of_opts`, `mk_audio_opts`, `mk_video_opts`, `filter_opts` called directly; `` `Int64`` and `` `Float`` values | —                                     |
| `HwContext`                                                                                                                                                                 | — (reachable only with NVENC)         |

### avcodec

| Area                                                                                                                                                                                                             | Coverage                  |
| ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------- |
| `version`, `flag_qscale`, `time_base`                                                                                                                                                                            | —                         |
| `params`, `descriptor` (of params)                                                                                                                                                                               | E 2.4, 4.4                |
| `capabilities`                                                                                                                                                                                                   | A 2.1                     |
| `name`, `hw_configs`                                                                                                                                                                                             | — (2.6 only with NVENC)   |
| `Packet`: `to_bytes`                                                                                                                                                                                             | E 2.4                     |
| `Packet`: `add_side_data`, `side_data`                                                                                                                                                                           | E 3.8                     |
| `Packet`: `dup`, `get_flags`, `get_size`, stream index, pts, dts, duration, position accessors, `create`, `content`                                                                                              | —                         |
| `Audio`: `find_encoder`, `find_decoder`, `get_id`, `string_of_id`                                                                                                                                                | A 2.1, 2.2                |
| `Audio`: `find_encoder_by_name`, `find_best_*`, `frame_size`, `create_decoder`, `codec_ids`, `encoders`, `decoders`, `descriptor`, `get_name`, `get_description`                                                 | E                         |
| `Audio`: `create_encoder`                                                                                                                                                                                        | A (options) 1.3; E 2.4    |
| `Audio`: `get_sample_rate`, `get_nb_channels` (params)                                                                                                                                                           | A 3.1 (positive)          |
| `Audio`: other params getters                                                                                                                                                                                    | E 3.1                     |
| `Audio`: `find_decoder_by_name`, `get_supported_*`, `sample_format` of a decoder, failed lookups                                                                                                                 | —                         |
| `Video`: `find_decoder`, `get_id`, `string_of_id`                                                                                                                                                                | A 2.2                     |
| `Video`: `get_width`, `get_height` (params)                                                                                                                                                                      | A 3.1 (positive)          |
| `Video`: `find_encoder_by_name`, `create_decoder`, `get_supported_pixel_formats`, `get_supported_color_spaces`, catalogue, other params getters                                                                  | E                         |
| `Video`: `create_encoder` (stand-alone), `find_encoder`, `find_decoder_by_name`, `get_supported_frame_rates`, `find_best_frame_rate`, `get_supported_color_ranges`, `find_best_pixel_format`, `get_pixel_aspect` | — (`create_encoder`: C)   |
| `Subtitle`: `find_decoder`, `get_id`, `string_of_id`                                                                                                                                                             | A 2.2                     |
| `Subtitle`: `find_encoder`, `find_encoder_by_name`, `get_params_id`, catalogue                                                                                                                                   | E 3.13, 2.3               |
| `Unknown`                                                                                                                                                                                                        | —                         |
| `BitstreamFilter`: `filters`                                                                                                                                                                                     | E 2.3                     |
| `BitstreamFilter`: `init`, `send_packet`, `send_eof`, `receive_packet`                                                                                                                                           | —                         |
| `decode`, `flush_decoder`                                                                                                                                                                                        | E 2.5 (frames discarded)  |
| `encode`, `flush_encoder`                                                                                                                                                                                        | E 2.4, checked downstream |

### av (libavformat)

| Area                                                                                                                                                                                              | Coverage                                       |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------- |
| `avformat_version`, `container_options`                                                                                                                                                           | —                                              |
| `Format`: `find_input_format`, `guess_output_format`                                                                                                                                              | E 5.5, 3.9                                     |
| `Format`: names, default codec ids                                                                                                                                                                | —                                              |
| `open_input`: plain, `opts`                                                                                                                                                                       | A 1.3                                          |
| `open_input`: `format`                                                                                                                                                                            | E 5.5                                          |
| `open_input`: `interrupt`                                                                                                                                                                         | A 3.14 (not on CI)                             |
| `open_input`: the three `configure_*_stream` callbacks; a failing open                                                                                                                            | —                                              |
| `open_input_stream` with `seek`                                                                                                                                                                   | E 3.10                                         |
| `open_input_stream` without `seek`, with `format` or `opts`; a raising callback                                                                                                                   | —                                              |
| `get_input_duration`, `get_input_metadata`                                                                                                                                                        | E 3.1, 3.12                                    |
| `get_input_format`, `set_input_metadata`                                                                                                                                                          | —                                              |
| `input_obj`                                                                                                                                                                                       | A 1.2                                          |
| `get_audio_streams`, `get_video_streams`, `get_subtitle_streams`                                                                                                                                  | A 3.1, 3.2, 3.6                                |
| `get_data_streams`, `find_best_subtitle_stream`                                                                                                                                                   | —                                              |
| `find_best_audio_stream`, `find_best_video_stream`                                                                                                                                                | E                                              |
| `get_input`, `get_output`, `get_codec_params`, `get_avg_frame_rate`, `set_avg_frame_rate`, `get_container_stream_time_base`, `get_frame_size`, `get_pixel_aspect`, `get_duration`, `get_metadata` | E                                              |
| `get_time_base`                                                                                                                                                                                   | A 3.4 (used to convert the asserted timestamp) |
| `get_index`, `set_time_base`                                                                                                                                                                      | —                                              |
| `read_input`: frame mode, audio/video/subtitle                                                                                                                                                    | A 3.1–3.6                                      |
| `read_input`: packet mode, audio/video/subtitle                                                                                                                                                   | E 3.8, 2.5                                     |
| `read_input`: `on_unhandled_packet`                                                                                                                                                               | A 3.3 (video kind only)                        |
| `read_input`: data packets, mixed packet and frame selection, no selection                                                                                                                        | —                                              |
| `seek`: `` `Millisecond``, defaults                                                                                                                                                               | A 3.4, 3.5                                     |
| `seek`: `flags`, `stream`, `min_ts`, `max_ts`, failure                                                                                                                                            | —                                              |
| `open_output`: plain, `opts`                                                                                                                                                                      | A 1.3                                          |
| `open_output`: `interrupt`                                                                                                                                                                        | A 3.14 (not on CI)                             |
| `open_output`: `format`, `interleaved`                                                                                                                                                            | —                                              |
| `open_output_format`                                                                                                                                                                              | — (C through device examples)                  |
| `open_output_stream` with `seek`, `opts`                                                                                                                                                          | A 3.9                                          |
| `reopen_output_stream`, `output_started`                                                                                                                                                          | —                                              |
| `set_output_metadata`, `set_metadata`                                                                                                                                                             | E 3.7 (not read back by an assertion)          |
| `new_stream_copy`                                                                                                                                                                                 | E 3.8                                          |
| `new_uninitialized_stream_copy`, `initialize_stream_copy`                                                                                                                                         | —                                              |
| `new_audio_stream`                                                                                                                                                                                | E; `opts` A 3.9                                |
| `new_video_stream`                                                                                                                                                                                | E 3.7; `hardware_context` —                    |
| `new_subtitle_stream`                                                                                                                                                                             | A 3.13                                         |
| `new_data_stream`, `codec_attr`, `bitrate`                                                                                                                                                        | —                                              |
| `write_packet`                                                                                                                                                                                    | E 3.8                                          |
| `write_frame`                                                                                                                                                                                     | E 3.7, checked downstream                      |
| `write_subtitle_frame`                                                                                                                                                                            | A 3.13                                         |
| `flush`, `tell`                                                                                                                                                                                   | —                                              |
| `close`                                                                                                                                                                                           | E everywhere; A after stream growth 3.6        |
| Use after `close`, double `close`, release by collection alone                                                                                                                                    | —                                              |

### avfilter

| Area                                                                                                                                    | Coverage                        |
| --------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------- |
| Sink getters `time_base`, `frame_rate`, `width`, `height`, `pixel_aspect`, `channels`, `channel_layout`, `sample_rate`, `sample_format` | E 4.2, 4.3                      |
| `pixel_format`                                                                                                                          | —                               |
| `set_frame_size`                                                                                                                        | E 4.3                           |
| `get_array_separator`                                                                                                                   | E 4.3 (either outcome accepted) |
| `filters`, `find`, `abuffer`, `buffer`, `abuffersink`, `buffersink`, `pad_name`                                                         | E 4.1–4.3                       |
| `find_opt`, `filter_name`, failed `find`                                                                                                | —                               |
| `init`, `attach`, `link`, `launch`, input and output handlers                                                                           | E 4.2, 4.3                      |
| `Exists` on a duplicate name                                                                                                            | —                               |
| `process_command`                                                                                                                       | —                               |
| `parse`                                                                                                                                 | —                               |
| `Utils.init_audio_converter`, `Utils.convert_audio`                                                                                     | E 4.4                           |
| `Utils.time_base`                                                                                                                       | —                               |

### avdevice

| Area                                                                     | Coverage   |
| ------------------------------------------------------------------------ | ---------- |
| `init`, format lists and defaults, the eight open functions              | — (some C) |
| `App_to_dev.control_messages`, `Dev_to_app.set_control_message_callback` | —          |

### swresample

| Area                                                                                                                                                                                                                                                                                                                    | Coverage                         |
| ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------- |
| `version`                                                                                                                                                                                                                                                                                                               | —                                |
| `Make(...).create`                                                                                                                                                                                                                                                                                                      | A 5.1–5.3                        |
| `create` with explicit `in_sample_format` / `out_sample_format`                                                                                                                                                                                                                                                         | E 5.3, 2.4                       |
| `create` failing on an undefined sample format                                                                                                                                                                                                                                                                          | —                                |
| `from_codec`                                                                                                                                                                                                                                                                                                            | A 5.4                            |
| `to_codec`                                                                                                                                                                                                                                                                                                              | E 5.5                            |
| `from_codec_to_codec`                                                                                                                                                                                                                                                                                                   | —                                |
| `convert`                                                                                                                                                                                                                                                                                                               | A 5.1–5.4                        |
| `convert` with `offset` and `length`                                                                                                                                                                                                                                                                                    | E 5.5                            |
| `flush`                                                                                                                                                                                                                                                                                                                 | —                                |
| Options: `` `Engine_soxr``                                                                                                                                                                                                                                                                                              | E 5.5; dither and filter types — |
| Content, per output kind, mono double                                                                                                                                                                                                                                                                                   | A 5.1 (planar frame: E)          |
| Content for more than one channel, or for any format other than double                                                                                                                                                                                                                                                  | —                                |
| Data modules used as input or output: `FloatArray`, `PlanarFloatArray`, `S32Bytes`, `DblBytes`, `DblPlanarBytes`, `U8BigArray`, `S32BigArray`, `DblBigArray`, `S32PlanarBigArray`, `FltPlanarBigArray`, `DblPlanarBigArray`, `Frame`, `S16Frame`, `S32Frame`, `DblFrame`, `DblPlanarFrame`                              | A or E                           |
| Data modules never instantiated: `Bytes`, `U8Bytes`, `S16Bytes`, `FltBytes`, `U8PlanarBytes`, `S16PlanarBytes`, `S32PlanarBytes`, `FltPlanarBytes`, `S16BigArray`, `FltBigArray`, `U8PlanarBigArray`, `S16PlanarBigArray`, `U8Frame`, `FltFrame`, `U8PlanarFrame`, `S16PlanarFrame`, `S32PlanarFrame`, `FltPlanarFrame` | —                                |

### swscale

| Area                                                     | Coverage           |
| -------------------------------------------------------- | ------------------ |
| `version`, `configuration`, `license`                    | —                  |
| Low-level `create` and `scale`                           | —                  |
| `Make(...).create`                                       | A 6.1; `threads` — |
| `convert`, `Bytes` → `Bytes`: plane count and sizes      | A 6.1              |
| `convert`: pixel content, output strides                 | —                  |
| `convert`, `Frame` → `BigArray`                          | E 6.2              |
| `PackedBigArray`, `Frame` as output, `BigArray` as input | —                  |
| Flags other than `Bilinear` and the empty list           | —                  |

### build

| Area                                      | Coverage                                                         |
| ----------------------------------------- | ---------------------------------------------------------------- |
| Detection of each library, version floors | —                                                                |
| The all-seven gate                        | 8.1, by construction                                             |
| Generated enumeration tables              | A 8.3, two enumerations of values; the rest E through catalogues |
| Behaviour with one library absent         | —                                                                |

## 12. Tests of these bindings elsewhere in the repository

The enclosing repository's own test directory has five OCaml programs that
name the binding modules. They test the host application's FFmpeg layer.
Their direct use of the bindings is: opening and closing an input, looking
up an input format by name, the rational record, and the error exception.
None asserts on a binding's behaviour. Nothing in the enclosing
repository refers to the `ffmpeg_citest` alias.
