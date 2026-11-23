# `%mp3`: MPEG audio Layer III through LAME

Part A. The writer of `%mp3`, `%mp3.cbr`, `%mp3.abr` and `%mp3.vbr`: one
MPEG-1, MPEG-2 or MPEG-2.5 Layer III elementary stream, with an ID3v2 tag
on a file and, in its file form, an encoder-information frame. It answers
[encoder.md §10](encoder.md#10-implementation-files) under the same
numbers.

This file also owns the rules every MPEG Layer III writer shares: the MPEG
rate and bit rate tables (§1.3), the access points of an MPEG audio stream
(§5), the header forms and the encoder-information frame (§6.1–§6.3), and
the ID3v2 tag rendered from metadata (§7.2). [shine.md](shine.md) and the
HLS packed-audio encoders
([encoder-packed-audio.md](../../outputs/hls/spec/encoder-packed-audio.md))
link them.

It relies on: the contract ([encoder.md](encoder.md)); format values, their
typing and validation phases ([formats.md](formats.md)); sinks
([sink.md](sink.md)); draining
([draining.md](../../source/spec/draining.md)); the fault classes
([faults.md](../../../spec/faults.md)).

## 1. Format and options

### 1.1 Names

`%mp3` and `%mp3.cbr` are the constant bit rate (CBR) mode, `%mp3.abr` the
average bit rate (ABR) mode, `%mp3.vbr` the variable bit rate (VBR) mode.
The four build one kind; the name fixes the mode, and no parameter changes
it.

### 1.2 Parameters

| Parameter                                                     | Names                  | Value                                                                | Default                                                  | Effect                                                                          |
| ------------------------------------------------------------- | ---------------------- | -------------------------------------------------------------------- | -------------------------------------------------------- | ------------------------------------------------------------------------------- |
| `mono`, `stereo` (anonymous); `mono=`, `stereo=`, `channels=` | all                    | channel count 1 or 2, literal ([formats.md §4](formats.md#4-typing)) | 2                                                        | one channel, or two                                                             |
| `stereo_mode`                                                 | all                    | `"default"`, `"stereo"`, `"joint_stereo"`                            | `"joint_stereo"`                                         | two-channel coding: the codec's choice, independent channels, or joint coding   |
| `internal_quality`                                            | all                    | int 0..9                                                             | 2                                                        | the codec's search effort: 0 best and slowest, 9 fastest                        |
| `samplerate`                                                  | all                    | an MPEG Layer III rate (§1.3)                                        | the frame's audio rate, deferred                         | the stream's rate; the writer converts from the frame's rate (§4)               |
| `bitrate`                                                     | `%mp3`, `%mp3.cbr`     | kbps, in the table of the rate's MPEG version (§1.3)                 | 128                                                      | the constant bit rate                                                           |
| `bitrate`                                                     | `%mp3.abr`             | the same                                                             | 128                                                      | the target mean bit rate                                                        |
| `quality`                                                     | `%mp3.vbr`             | int 0..9                                                             | 4                                                        | the VBR quality: 0 highest bit rate, 9 lowest                                   |
| `min_bitrate`, `max_bitrate`                                  | `%mp3.abr`, `%mp3.vbr` | kbps, as `bitrate`                                                   | unset (the codec's limits)                               | every frame's bit rate stays within them                                        |
| `hard_min`                                                    | `%mp3.abr`, `%mp3.vbr` | bool                                                                 | false                                                    | `min_bitrate` holds for silent frames too                                       |
| `id3v2`                                                       | all                    | `true` (= 3), `false`, `"none"`, `3`, `4`                            | adapted: version 3 on a file sink, none elsewhere (§6.4) | an ID3v2 tag of that version as the stream's first preamble, or none (§6.4, §7) |

- `stereo_mode` set explicitly with one channel is rejected: it has no
  effect on a mono stream
  ([formats.md §3](formats.md#3-the-parameter-specification)).
- `%mp3.vbr` takes no `bitrate`: a mean bit rate has no effect in VBR mode.
  `%mp3.abr` takes no `quality`.
- `min_bitrate ≤ bitrate ≤ max_bitrate`, for those set. A configuration
  where they cross is rejected.
- `id3v2` takes no other version. Version 2 has a different frame layout
  (three-letter identifiers, 24-bit sizes) that this writer does not render.
- `%mp3` is printed with its mode's name and every parameter of its
  configuration; `id3v2` is printed only when set
  ([formats.md §10](formats.md#10-equality-printing-serialisation)).

### 1.3 MPEG Layer III tables

| MPEG version | Rates (Hz)          | Bit rates (kbps)                                              | Samples per frame `F` |
| ------------ | ------------------- | ------------------------------------------------------------- | --------------------- |
| 1            | 32000, 44100, 48000 | 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320 | 1152                  |
| 2            | 16000, 22050, 24000 | 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160     | 576                   |
| 2.5          | 8000, 11025, 12000  | the same as MPEG-2                                            | 576                   |

The rate decides the version. Every bit rate parameter is checked against
the table of that version. Reason: a bit rate outside it cannot be written
in a frame header, and a writer that substitutes another rate silently gives
a stream the script did not ask for.

### 1.4 When each check runs

Per [formats.md §7](formats.md#7-when-a-format-is-validated):

| Check                                                                                                                                    | Phase                                |
| ---------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------ |
| channel count                                                                                                                            | type checking                        |
| every literal value above, and their combinations (bit rates against a literal rate, `min ≤ mean ≤ max`, `stereo_mode` with one channel) | type checking                        |
| the same, for computed values                                                                                                            | evaluation of the format expression  |
| a deferred rate, and the bit rates against it                                                                                            | start-up, before any writer is built |
| the format against the output ([encoder.md §5.3](encoder.md#53-the-check), §2)                                                           | construction of the output           |

None of these consults the codec library: they are facts of the MPEG
format. A configuration that passed them and that the library still refuses
at creation is a hard configuration error (§15).

### 1.5 Helpers

| Helper      | Value                                                               |
| ----------- | ------------------------------------------------------------------- |
| `extension` | `mp3`                                                               |
| `mime`      | `audio/mpeg`                                                        |
| `bitrate`   | CBR and ABR: `bitrate × 1000`; VBR: none                            |
| `codecs`    | `mp4a.40.34`, the value HLS players expect for MPEG Layer III audio |

## 2. Declarations

| Declaration        | Value                                           |
| ------------------ | ----------------------------------------------- |
| streamable         | yes: every frame is an access point (§5)        |
| needs seekable     | no                                              |
| can continue       | yes: MPEG frames only (§6.3)                    |
| in-band metadata   | no                                              |
| segmentable        | yes, through the HLS packed-audio encoder (§12) |
| needs no byte sink | no                                              |
| accepts delegated  | no                                              |

Adapted defaults ([encoder.md §5.3](encoder.md#53-the-check)): the
encoder-information frame is written in the file form only (§6.1); the
ID3v2 tag, version 3, is written by default on a file sink only, under the
tag rule of [encoder.md §3.5](encoder.md#35-header-forms-and-preambles),
item 7.

Explicit parameters refused against the declared capabilities, at the
output's construction: `id3v2` set to a version, for an output whose sinks
are `live`, `continuing` or `segmented`. `id3v2=false` or `"none"` is
accepted everywhere. Reason: a preamble on a live sink
reaches readers in the middle of the stream, on a continuing sink it lands
in the middle of the file, and a segmented sink starts every segment with
the ID3 tag the HLS contract owns.

## 3. Creation

- Inputs: the configuration, the confirmed capabilities, the initial
  metadata (used only for the ID3v2 tag), the output's name and the format's
  position.
- The codec is configured with the input rate (the frame's), the output
  rate, the channel count and mode, the mode's bit rate parameters and the
  algorithm quality, and its acceptance is checked before the writer is
  returned. A refusal is a hard configuration error naming the output, the
  parameter the library names, and the format's position.
- The form is chosen once, from the capabilities (§6.1).
- The writer reads `F` and the codec's encoder delay once the codec is
  configured (§8).
- Creation writes nothing into the sink.

## 4. Input

- The writer reads the piece's audio only, as floating-point samples. Each
  sample is clipped to [−1, 1]; NaN is taken as 0.
- When the stream's rate differs from the frame's, a rate conversion runs
  before the codec, as a stage of
  [draining.md §1](../../source/spec/draining.md#1-stages): the writer's own
  converter or the codec's, drained at `end` either way (§9) and counted in
  the signalled delay (§8).
- The writer hands the codec any number of samples per call. What it holds
  between calls, outside the codec, is less than one frame of `F` samples.
  No result depends on the piece boundaries
  ([encoder.md §3.3](encoder.md#33-frames-in)).

## 5. Bytes and markers

Layout of a stream:

```
[ID3v2 tag]                   preamble: by default on a file sink, or as id3v2 sets it (§6.4, §7)
[encoder-information frame]   preamble, file form only (§6.2)
MPEG audio frame × n          Layer III, the configured version, rate and mode
```

Markers, for every MPEG Layer III writer:

- `stream_start(0, t₀)` before the first byte: the header is empty.
- `access_point(t)` before every audio frame, at its header's start code. None is
  placed before or inside a preamble.
- `t` is the media time of the first input sample the frame decodes to:
  `t₀ + (k × F − delay) / rate` for the `k`-th audio frame (from 0), delay
  as in §8, never below `t₀`.
- A reader starting at an access point decodes from that frame. A frame
  whose main data begins in earlier frames (the bit reservoir) decodes as
  silence for a reader that lacks them: at most the first `F` samples a
  mid-stream reader decodes are silent. This is how MPEG audio is read from
  the middle and is not a decoding failure.
- Frame headers carry no CRC; `original` is 1, `copyright` 0, emphasis
  none.

## 6. Header forms and preambles

### 6.1 Forms

| Sink                                                                          | Form       | Before the first audio frame                                                                                                    |
| ----------------------------------------------------------------------------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------- |
| `seekable`, `fresh` (a file sink)                                             | file       | the ID3v2 tag unless `id3v2` turns it off, the encoder-information frame: placeholder first, final values at a clean end (§6.2) |
| fresh, not seekable, neither live nor segmented (a FIFO, a device, a process) | streaming  | the ID3v2 tag only when `id3v2` sets a version                                                                                  |
| `live` or `segmented`                                                         | streaming  | nothing                                                                                                                         |
| `continuing`                                                                  | continuing | nothing (§6.3)                                                                                                                  |

Raw MPEG audio has no header field for its length: its streaming form is
the stream itself, and it signals no delay or padding
([encoder.md §3.7](encoder.md#37-delay-and-padding)).

### 6.2 The encoder-information frame

The frame follows the Xing and LAME tag layout:

1. It is an MPEG audio frame of the stream's version, rate and channel mode
   that carries no audio, placed after any ID3v2 tag and before the first
   audio frame. Its bit rate is the stream's for CBR, so that a CBR stream
   stays constant; for ABR and VBR, the lowest bit rate of the version whose
   frame holds the tag. No audio frame's main data begins in it.
2. Its tag identifier is `Info` for CBR and `Xing` for ABR and VBR.
3. Every Xing field is present: the frame count (the audio frames after this
   one), the byte count (from this frame's first byte to the end of the last
   audio frame), a 100-entry table of contents, and the quality indicator.
4. The LAME extension follows: the encoder's short identifier (9 bytes, §8),
   the bit rate mode, the lowpass, the encoder delay and the padding (12 bits
   each, samples at the stream's rate, §8), the music length (equal to the
   byte count), the CRC-16 of the audio frames' bytes, and the CRC-16 of the
   frame's first 190 bytes. Fields the writer has no value for (replay gain,
   preset) are zero.
5. **Placeholder.** The frame written first holds the delay, a padding of
   0, counts and table of contents of 0, and CRCs valid for what it holds. A
   zero count is read by decoders as absent, so a file whose writer was
   abandoned, or whose process died, decodes in full with its delay
   trimmed.
6. At a clean end, the writer seeks to the frame and rewrites it with the
   final counts, table of contents, padding and CRCs. The frame keeps its
   size.
7. The table of contents is computed from frame positions the writer
   records while it streams, in memory bounded independently of the
   stream's length.

### 6.3 Continuing

On a `continuing` sink, the writer writes audio frames only: no ID3v2 tag,
no encoder-information frame
([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)). It takes the
existing stream to be of its own version, rate and channel mode.

At a clean end, the sink being seekable, the writer SHOULD read the start of
the existing stream (skipping an ID3v2 tag by its declared size), and when
it finds an encoder-information frame of its version, rate and channel mode:

- add its own frames and bytes to the frame count, the byte count and the
  music length;
- recompute the table of contents over the whole stream, or, where it
  cannot, clear the table's presence flag in place, so that no reader
  follows a stale table;
- set the padding to its own stream's: the junction keeps the first
  stream's padding and the appended stream's delay as decoded samples;
- recompute the audio CRC from the frames read back, and the tag's CRC.

An existing stream with no encoder-information frame is left as it is. A
frame of another version, rate or channel mode is left unchanged, logged at
`severe` with the file's name.

### 6.4 ID3v2 as a preamble

The ID3v2 tag describes the whole file and precedes the stream's first
media: it is a preamble, never part of an entry header. It is written at
the stream's first byte, only when it has a frame (§7.1): by default on a
file sink, in version 3; with `id3v2` set to a version, on every fresh sink
that is neither live nor segmented (§2); never with `id3v2=false` or
`"none"`. A cover in the initial metadata becomes an `APIC` frame
(§7.2).

## 7. Metadata

### 7.1 Initial metadata and in-band offers

- The initial metadata ([encoder.md §2.2](encoder.md#22-creation)) fills the
  ID3v2 tag when one is written (§6.4), and is used for nothing else.
- When the initial metadata gives no frame (it is empty, or no key maps),
  no tag is written ([encoder.md §9](encoder.md#9-in-band-metadata), 4).
- The writer offers no in-band metadata.

### 7.2 ID3v2 rendering

Other writers of ID3v2 tags from exported metadata link this section.

- Header: `ID3`, the major version (3 or 4), revision 0, flags 0, the size
  of the frames as a 28-bit synchsafe integer. No extended header, no
  footer.
- One frame per metadata entry whose key maps:

  | Key                                                                                                        | Frame, v2.3 | Frame, v2.4 |
  | ---------------------------------------------------------------------------------------------------------- | ----------- | ----------- |
  | `title`                                                                                                    | `TIT2`      | `TIT2`      |
  | `artist`                                                                                                   | `TPE1`      | `TPE1`      |
  | `album`                                                                                                    | `TALB`      | `TALB`      |
  | `band`, `albumartist`                                                                                      | `TPE2`      | `TPE2`      |
  | `genre`, `content`                                                                                         | `TCON`      | `TCON`      |
  | `composer`                                                                                                 | `TCOM`      | `TCOM`      |
  | `performer`                                                                                                | `TOPE`      | `TOPE`      |
  | `publisher`                                                                                                | `TPUB`      | `TPUB`      |
  | `copyright`                                                                                                | `TCOP`      | `TCOP`      |
  | `tracknumber`                                                                                              | `TRCK`      | `TRCK`      |
  | `bpm`                                                                                                      | `TBPM`      | `TBPM`      |
  | `language`                                                                                                 | `TLAN`      | `TLAN`      |
  | `encoder`                                                                                                  | `TENC`      | `TENC`      |
  | `year`, `date`                                                                                             | `TYER`      | `TDRC`      |
  | `comment`                                                                                                  | `COMM`      | `COMM`      |
  | `url`                                                                                                      | `WXXX`      | `WXXX`      |
  | a cover key ([output-encoded.md §4.2](output-encoded.md#42-the-exported-form)) holding a picture           | `APIC`      | `APIC`      |
  | a key that is itself a text frame identifier of the version (`T` and three capitals or digits, not `TXXX`) | that frame  | that frame  |

  Other keys are not written. When several entries give one frame, the last
  one wins.

- Text frames: an encoding byte, then the text. Version 3: encoding 1,
  UTF-16 with a byte-order mark. Version 4: encoding 3, UTF-8. `COMM`: the
  encoding byte, the language `und`, an empty description, then the text.
  `WXXX`: the encoding byte, an empty description, then the URL in
  ISO-8859-1. `APIC`: the encoding byte, the picture's MIME type
  (`image/jpeg` or `image/png`, read from its first bytes), picture type
  3 (front cover), an empty description, then the picture; a FLAC picture
  block (`metadata_block_picture`) is unpacked to its picture first. A
  value that is not a JPEG or PNG picture is not written.
- Frame sizes are 32-bit big-endian in version 3, synchsafe in version 4.
- Frame status flags are 0, except "discard when the file is altered" on
  `TENC` and `TLEN`, as both versions recommend.
- A standard ID3v2 parser of the declared version reads every frame.

## 8. Delay and padding

- **Delay**: the codec's encoder delay, read from the library once it is
  configured (576 samples for LAME at its usual settings), plus the delay of
  a rate conversion before the codec, in samples at the stream's rate.
- **Padding**: at the stream's end, `n × F − delay − input`, where `n` is
  the number of audio frames and `input` the number of input samples at the
  stream's rate.
- Both are signalled in the encoder-information frame of the file form
  (§6.2), and updated for an appended stream (§6.3). The streaming form has
  no field for them.
- **Short identifier.** Every MPEG Layer III writer's encoder-information
  frame carries a LAME-compatible short identifier: 9 bytes beginning with
  `LAME`, then a version string. Reason: decoders read the delay and
  padding fields only after an identifier they know; FFmpeg's mp3 demuxer
  trusts them only after a `LAME`, `Lavf` or `Lavc` identifier, and
  ignores them otherwise, which leaves the delay and the padding in the
  decoded audio. This writer's identifier is the library's (`LAME` and its
  version).
- Each file of a split has its own writer and signals its own values
  ([encoder.md §2.1](encoder.md#21-one-writer-per-destination-stream)).
  Gapless splits are not offered.

## 9. End

`end(clean)`, upstream first:

1. the rate conversion is told the input ended, and everything it returns
   goes to the codec;
2. every sample the writer holds goes to the codec, the last partial frame
   included;
3. the codec is flushed: it encodes its look-ahead and its last frame,
   padded with silence, and every byte it returns is written;
4. on a `seekable` sink, the encoder-information frame is rewritten (§6.2),
   or the existing one updated on a continuing sink (§6.3);
5. `end` is written.

No input sample is lost: decoding the stream and trimming the signalled
delay and padding gives exactly the input sample count at the stream's
rate.

A writer ended before any piece writes nothing, ID3v2 tag included
([encoder.md §2.3](encoder.md#23-states)). `end(abandoned)` releases the
codec and writes nothing more. The codec's context is released exactly once,
at `end`.

## 10. Properties

| Property  | Value                                                | Known         |
| --------- | ---------------------------------------------------- | ------------- |
| `bitrate` | CBR: the bit rate; ABR: the mean bit rate; VBR: none | from creation |
| `delay`   | §8                                                   | from creation |
| `padding` | §8                                                   | at the end    |

`bitrate` equals the helper's (§1.5).

## 11. Live latency

The writer holds less than one frame of samples besides the codec's
look-ahead, which is not counted against `L`
([encoder.md §3.9](encoder.md#39-live-latency-bound)). Every frame is
written as the codec returns it; nothing else holds bytes.

## 12. Segmenting face

[encoder-packed-audio.md](../../outputs/hls/spec/encoder-packed-audio.md).
On a segmented sink the writer writes no preamble (§2), and its access
points are the frame boundaries of §5.

## 13. Delegated destination

Nothing.

## 14. Concurrency

- No work of its own: every call runs on the output's streaming work
  ([encoder.md §7](encoder.md#7-concurrency)).
- One codec context per writer. State the library shares between contexts
  (tables built once per process) is initialised before any writer is
  created, so that writers of several outputs run on several domains
  without coordination.

## 15. Faults

| Fault                                                                                                                       | Class               | When                                |
| --------------------------------------------------------------------------------------------------------------------------- | ------------------- | ----------------------------------- |
| a value out of its range or table; crossing bit rates; `stereo_mode` with one channel; an `id3v2` version other than 3 or 4 | configuration error | type checking or evaluation (§1.4)  |
| a deferred rate outside the table, or a bit rate outside its table                                                          | configuration error | start-up                            |
| `id3v2` set for a live, continuing or segmented output                                                                      | configuration error | construction of the output          |
| the library refuses the configuration                                                                                       | configuration error | creation; hard error                |
| the library fails to encode or to flush while streaming, or memory runs out                                                 | output fault        | the writer becomes `failed`         |
| reading back or rewriting the encoder-information frame fails at a clean end                                                | not escalated       | logged at `severe`; the end goes on |

Every error names the parameter or the library's reason, the output and the
format's position; none is only printed on a console.

## 16. Tests

Harness: a finite source of a known sample count, with a click at a known
sample and a distinctive tone in its last 100 ms; a decoder that reports raw
sample counts and one that honours the encoder-information frame; an MPEG
frame walker; an ID3v2 parser.

1. For each mode, at 44 100 Hz and at 22 050 Hz from a 44 100 Hz frame: a
   file written to a seekable fresh sink has an encoder-information frame
   whose frame count equals the walker's, whose byte count and music length
   equal the stream's size, and whose CRCs verify; decoding with the frame
   honoured gives exactly the input sample count, the click at its input
   index to within one sample, and the final tone.
2. The same configuration through a FIFO and through a live output: no
   encoder-information frame and no ID3v2 tag; every frame header valid.
   `%mp3(id3v2=3)` through the FIFO: an ID3v2 tag, no encoder-information
   frame.
3. A writer abandoned after 1 s on a seekable sink: the file decodes with
   its placeholder frame, its delay trimmed.
4. Pieces of random lengths (from 1 sample) and offsets give the same
   decoded sample count as one piece.
5. One minute appended to a one-minute file of the same configuration: one
   encoder-information frame, counts covering both minutes, a decode of two
   minutes plus the junction's padding and delay; no ID3v2 tag in the
   middle.
6. `%mp3(samplerate=22050, bitrate=320)`, `%mp3.vbr(min_bitrate=320,
max_bitrate=8)`, `%mp3(id3v2=2)`, `%mp3(mono, stereo_mode="stereo")` and
   `%mp3.vbr(bitrate=128)` fail `--check` with a position;
   `%mp3(channels=1)` types and builds mono.
7. `%mp3` to `output.file`, and `%mp3(id3v2=4)`, with initial metadata
   `title="Hé", artist="A"` and a cover: a standard parser reads `TIT2`,
   `TPE1` and `APIC` with those values, in versions 3 and 4; the tag
   precedes the encoder-information frame. With empty initial metadata: no
   tag. `%mp3(id3v2=false)` to `output.file`: no tag.
8. `%mp3(id3v2=true)` given to `output.icecast`, to `output.file` with
   `append=true` and to an HLS output is refused at evaluation, naming
   `id3v2`; `%mp3` given to the same outputs is accepted and writes no
   preamble.
9. A shared harbor reader starting after 3 s receives bytes that begin at a
   frame header and decode, losing at most `F` samples.
10. Two runs differing only in one of `stereo_mode`, `internal_quality`,
    `quality`, `min_bitrate`, `max_bitrate`, `hard_min` give bytes that
    differ as the option documents.
11. Thirty-two writers in parallel give the bytes the same writers give one
    after the other.
