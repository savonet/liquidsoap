# `%shine`: fixed-point MPEG audio Layer III

Part A. The writer of `%shine` and its alias `%mp3.fxp`: one constant bit
rate MPEG Layer III elementary stream, encoded by libshine, with an ID3v2
tag on a file and an encoder-information frame in its file form. It answers
[encoder.md §10](encoder.md#10-implementation-files) under the same
numbers.

It relies on: the MPEG Layer III rules [mp3.md](mp3.md) owns (the rate and
bit rate tables §1.3, the access points §5, the forms and the
encoder-information frame §6.1–§6.4, the ID3v2 rendering §7.2); the
contract ([encoder.md](encoder.md)), whose §3.3 owns the conversion of
samples to integers; format values ([formats.md](formats.md)); sinks
([sink.md](sink.md)); draining
([draining.md](../../source/spec/draining.md)); the fault classes
([faults.md](../../../spec/faults.md)).

## 1. Format and options

`%shine(...)` and `%mp3.fxp(...)` build the same format, printed as
`%shine(...)`.

| Parameter                                                     | Value                                                    | Default                          | Effect                |
| ------------------------------------------------------------- | -------------------------------------------------------- | -------------------------------- | --------------------- |
| `mono`, `stereo` (anonymous); `mono=`, `stereo=`, `channels=` | channel count 1 or 2, literal                            | 2                                | mono, or joint stereo |
| `samplerate`                                                  | a rate of [mp3.md §1.3](mp3.md#13-mpeg-layer-iii-tables) | the frame's audio rate, deferred | the stream's rate     |
| `bitrate`                                                     | kbps, in the table of the rate's MPEG version            | 128                              | the constant bit rate |
| `id3v2`                                                       | as [mp3.md §1.2](mp3.md#12-parameters)                   | adapted, as `%mp3`'s             | as `%mp3`'s (§6, §7)  |

Checks, per [formats.md §7](formats.md#7-when-a-format-is-validated):

| Check                                                                                     | Phase                                       |
| ----------------------------------------------------------------------------------------- | ------------------------------------------- |
| channel count                                                                             | type checking                               |
| a literal rate and bit rate against the MPEG tables                                       | type checking                               |
| the same, computed                                                                        | evaluation                                  |
| the (rate, bit rate) pair against what the library encodes, by its side-effect-free query | evaluation; at start-up for a deferred rate |
| the format against the output (§2)                                                        | construction of the output                  |

Helpers: `extension` `mp3`; `mime` `audio/mpeg`; `bitrate` `bitrate × 1000`;
`codecs` `mp4a.40.34`.

## 2. Declarations

As [mp3.md §2](mp3.md#2-declarations): streamable, can continue (MPEG
frames only), segmentable through the HLS packed-audio encoder; it needs no
seekable destination, offers no in-band metadata, needs a byte sink and
accepts no delegated destination.

Adapted defaults and refused explicit parameters: those of `%mp3`
([mp3.md §2](mp3.md#2-declarations)): the encoder-information frame in the
file form only, the ID3v2 tag by default on a file sink only, and `id3v2`
set to a version refused for a live, continuing or segmented output.

## 3. Creation

- Inputs: the configuration, the confirmed capabilities, the initial
  metadata (for the ID3v2 tag only), the output's name and the format's
  position.
- The library is initialised with the rate, the bit rate, the channel count
  and the mode (mono, or joint stereo). A refusal or a failed
  initialisation is a hard configuration error naming the output, the
  library's reason and the format's position.
- The form is chosen once, from the capabilities, as
  [mp3.md §6.1](mp3.md#61-forms) states.
- Creation writes nothing into the sink.

## 4. Input

- The piece's audio is converted from the frame's rate to the stream's by
  the writer's rate converter, a stage of
  [draining.md §1](../../source/spec/draining.md#1-stages), then converted to
  signed 16-bit integers as [encoder.md §3.3](encoder.md#33-frames-in)
  states.
- The library encodes passes of exactly `F` samples (1152 for MPEG-1, 576
  otherwise). The writer holds fewer than `F` samples between calls. No
  result depends on the piece boundaries
  ([encoder.md §3.3](encoder.md#33-frames-in)).

## 5. Bytes and markers

[mp3.md §5](mp3.md#5-bytes-and-markers):

```
[ID3v2 tag]                   preamble, as mp3.md §6.4
[encoder-information frame]   file form only
MPEG audio frame × n          Layer III, CBR, mono or joint stereo
```

## 6. Header forms and preambles

[mp3.md §6.1–§6.4](mp3.md#6-header-forms-and-preambles): the
encoder-information frame in the file form, written as a placeholder then
rewritten at a clean end; none in the streaming form; on a continuing sink,
frames only, and the existing frame's counts updated at a clean end. The
ID3v2 tag as [mp3.md §6.4](mp3.md#64-id3v2-as-a-preamble).

## 7. Metadata

- Initial metadata: the ID3v2 tag, as
  [mp3.md §7](mp3.md#7-metadata) states.
- The writer offers no in-band metadata.

## 8. Delay and padding

- **Delay**: libshine's analysis delay, 528 samples, plus the delay of the
  rate converter, in samples at the stream's rate. The library offers no
  query for it: 528 is the declared constant
  [encoder.md §3.7](encoder.md#37-delay-and-padding) allows, a fact of its
  filter bank, and a library whose delay differs fails §16 test 1.
- **Padding**: `n × F − delay − input`, as in
  [mp3.md §8](mp3.md#8-delay-and-padding).
- Both are signalled in the encoder-information frame of the file form,
  under the LAME-compatible short identifier of
  [mp3.md §8](mp3.md#8-delay-and-padding), so that decoders read them;
  the encoder's name, libshine and its version, is in the ID3v2 tag's
  `TENC` frame when a tag is written.
- Each file of a split has its own writer. Gapless splits are not offered.

## 9. End

`end(clean)`, upstream first:

1. the rate converter is told the input ended; what it returns joins the
   held samples;
2. the held samples are padded with silence to a full pass and encoded;
3. passes of silence follow until every input sample has left the
   library's delay line: the frames written then number at least
   `⌈(input + delay) / F⌉`;
4. the library's remaining bits are flushed and written;
5. on a `seekable` sink, the encoder-information frame is rewritten, or the
   existing one updated on a continuing sink;
6. `end` is written.

The library's flush emits only buffered bits, never audio: the silent passes
are how its delay line is drained. No input sample is lost: decoding and
trimming the signalled delay and padding gives exactly the input sample
count at the stream's rate.

A writer ended before any piece writes nothing. `end(abandoned)` releases
the library and the converter and writes nothing more. The library handle
is released exactly once, at `end`.

## 10. Properties

| Property  | Value            | Known         |
| --------- | ---------------- | ------------- |
| `bitrate` | `bitrate × 1000` | from creation |
| `delay`   | §8               | from creation |
| `padding` | §8               | at the end    |

## 11. Live latency

As [mp3.md §11](mp3.md#11-live-latency): fewer than `F` held samples besides
the library's delay line; every frame written as the library returns it.

## 12. Segmenting face

[encoder-packed-audio.md](../../outputs/hls/spec/encoder-packed-audio.md).

## 13. Delegated destination

Nothing.

## 14. Concurrency

No work of its own. One library handle per writer; writers of several
outputs run on several domains without coordination.

## 15. Faults

| Fault                                                                        | Class               | When                                |
| ---------------------------------------------------------------------------- | ------------------- | ----------------------------------- |
| channel count, a rate or bit rate outside the MPEG tables                    | configuration error | type checking or evaluation         |
| a (rate, bit rate) pair the library does not encode                          | configuration error | evaluation, or start-up if deferred |
| the library refuses its initialisation                                       | configuration error | creation; hard error                |
| the library fails while streaming, or memory runs out                        | output fault        | the writer becomes `failed`         |
| reading back or rewriting the encoder-information frame fails at a clean end | not escalated       | logged at `severe`                  |

## 16. Tests

Harness as [mp3.md §16](mp3.md#16-tests).

1. At 44 100 Hz, and at 48 000 Hz from a 44 100 Hz frame: the file's
   encoder-information frame verifies; decoding it with delay and padding
   trimmed gives exactly the input sample count, a click at its input
   index to within one sample, and the final tone.
2. A FIFO and a live output: no encoder-information frame and no ID3v2
   tag; every frame header valid, every frame at the configured bit rate.
   On a file: an ID3v2 tag carrying the title, then the
   encoder-information frame, whose identifier begins with `LAME`; FFmpeg
   reports the delay and padding it signals.
3. Pieces of random lengths and offsets give the same decoded sample count
   as one piece.
4. `%shine(samplerate=12345)`, `%shine(bitrate=7)` and `%shine(channels=6)`
   fail `--check` with a position; `%mp3.fxp` prints as `%shine(...)`.
5. One minute appended to a one-minute file: one encoder-information frame,
   counts covering both minutes.
6. A shared harbor reader starting after 3 s decodes from its first byte.
