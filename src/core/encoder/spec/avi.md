# `%avi`

Part A. The AVI writer: a RIFF/AVI stream with one uncompressed I420 video
stream and one 16-bit PCM audio stream, interleaved in a `movi` list. This
file owns `%avi`'s parameter specification, its byte layout, and what the
writer does with each capability. It answers every section of
[encoder.md §10](encoder.md#10-implementation-files) under the same number.

It relies on: the seam ([encoder.md](encoder.md)); format values and their
phases ([formats.md](formats.md)); sinks ([sink.md](sink.md)); draining
([draining.md](../../source/spec/draining.md)); the fault classes
([faults.md](../../../spec/faults.md)).

The AVI streaming form of §6 is also what [`%external`](external.md) sends to
its process when it has video.

`%avi` MAY be provided by the FFmpeg writer
([ffmpeg.md](ffmpeg.md)), as the AVI muxer with `rawvideo` in `yuv420p` and
`pcm_s16le`, with the options of §1 mapped onto it. Every rule of this file
binds that implementation too.

## 1. Format and options

`%avi(...)`. Parameters, folded in order, last one wins
([formats.md §3](formats.md#3-the-parameter-specification)):

| Parameter                                            | Value           | Default                                             | Effect                                                              |
| ---------------------------------------------------- | --------------- | --------------------------------------------------- | ------------------------------------------------------------------- |
| `mono`, `stereo`, `mono=b`, `stereo=b`, `channels=n` | literal         | 2 channels                                          | channel count, type-deciding ([formats.md §4](formats.md#4-typing)) |
| `samplerate=n`                                       | int, 1 to 2³²−1 | `frame.audio.samplerate`, deferred                  | audio sample rate                                                   |
| `width=n`, `height=n`                                | int ≥ 1         | `frame.video.width`, `frame.video.height`, deferred | picture size                                                        |

- The type is `format(audio=pcm(<layout>), video=<default>)`
  ([formats.md §4](formats.md#4-typing)).
- The video rate is `frame.video.framerate`, deferred; it is not a
  parameter.
- The audio byte rate `2 × channels × samplerate` MUST fit in 32 bits, as in
  [wav.md §1](wav.md#1-format-and-options).
- Out-of-range values are errors at the phase of
  [formats.md §7](formats.md#7-when-a-format-is-validated), naming the
  parameter. Any other parameter is rejected.
- **No adaptable parameter.** The header form (§6) and the `INFO` tags (§7)
  follow the capabilities.
- Helpers ([formats.md §9](formats.md#9-helpers)): extension `avi`, MIME
  `video/avi`; `codecs` none; `bitrate` none (the video rate is always
  deferred).

## 2. Declarations

| Declaration        | Value                                             |
| ------------------ | ------------------------------------------------- |
| streamable         | yes; every chunk boundary is an access point (§5) |
| needs seekable     | no                                                |
| can continue       | no                                                |
| in-band metadata   | no                                                |
| segmentable        | no                                                |
| needs no byte sink | no                                                |
| accepts delegated  | no                                                |

`%avi` cannot continue: a second AVI header in the middle of a file is not a
valid AVI, and AVI has no media-only extension of an existing stream. An
output that declares a continuing sink refuses it at evaluation
([encoder.md §5.3](encoder.md#53-the-check)). Migration: write each run to
its own file, or use a format that can continue.

## 3. Creation

- Inputs used: the configuration, the capabilities, the sink, the output's
  name (logs), the initial metadata (§7).
- The header form is chosen from the capabilities (§6). Nothing is started.
  Creation logs at `info` the picture size, frame rate, channels and sample
  rate.
- When the configuration's rate differs from the frame's audio rate, a
  sample-rate converter is created.

## 4. Input

- **Audio**: the piece's audio track, converted to the configured rate (a
  stage of [draining.md §1](../../source/spec/draining.md#1-stages)), clipped
  to 16-bit integers as [encoder.md §3.3](encoder.md#33-frames-in) states,
  little endian, and interleaved in the layout's order.
- **Video**: each image of the piece, in order. An image whose size differs
  from the configured one is scaled to fit it, keeping its proportions, and
  centred on a black picture of the configured size; an image with alpha is
  composited over black. Strides of the input never reach the bytes.
  Reason: the header declares one picture size for the whole stream
  ([encoder.md §3.3](encoder.md#33-frames-in)).
- An image is written as its Y plane (`width × height` bytes) then its U and
  V planes (`⌈width/2⌉ × ⌈height/2⌉` bytes each), rows unpadded, top row
  first. The picture's size in bytes is
  `width × height + 2 × ⌈width/2⌉ × ⌈height/2⌉`, odd sizes included.
- Decoded audio and pictures do not depend on piece boundaries.

## 5. Bytes and markers

The stream is the header (§6), then chunks in the `movi` list:

- one `00db` chunk per image, holding exactly one picture;
- one `01wb` chunk per piece that holds audio, holding its samples;
- within a piece, the image chunks in order, then the audio chunk. A piece
  with no image writes no `00db` chunk, and a piece with no sample writes no
  `01wb` chunk.

A chunk is its identifier, its payload length (32 bits, little endian), the
payload, and one zero byte when the length is odd.

- `stream_start(header_length, 0)` precedes the header; `header_length` is
  the length of everything before the first chunk of `movi`.
- An `access_point(t)` precedes every chunk. Reason: both streams are made
  only of key units, so a reader starting at any chunk decodes from there
  given the header. The entry header is the header in its streaming form.

## 6. Header forms and preambles

**Header**, in order (all integers little endian):

1. `RIFF`, its size, `AVI `;
2. `LIST hdrl`:
   - `avih`: microseconds per frame (`1 000 000 / framerate`, rounded),
     the interleaved flag, total frames, 2 streams, width, height;
   - `LIST strl` for video: `strh` (`vids`, handler `I420`, scale 1, rate
     `framerate`, length in frames, the picture rectangle), `strf` (a
     BITMAPINFOHEADER: width, positive height, 1 plane, 12 bits, compression
     `I420`, image size the picture's size of §4);
   - `LIST strl` for audio: `strh` (`auds`, handler 1, scale 1, rate
     `samplerate`, length in samples, sample size `2 × channels`), `strf` (a
     WAVEFORMATEX: tag 1, channels, rate, byte rate, block align
     `2 × channels`, 16 bits);
3. `LIST INFO` with `ISFT` (the software's name and version) and the initial
   metadata (§7);
4. `LIST`, its size, `movi`.

**Streaming form** (every fresh sink, and the first write on a seekable
one): the RIFF and `movi` sizes are 0xFFFFFFFF; total frames and stream
lengths are 0.

**No index.** The writer writes no `idx1` index and no OpenDML extension,
on any sink. Reason: an index means reading back or holding every chunk's
position; AVI readers read an unindexed stream sequentially.

**File form** (fresh seekable sink): at a clean end, the writer MAY seek
back and write the RIFF and `movi` sizes, the total frames and the stream
lengths, from counts it keeps while streaming (nothing it holds grows with
the stream's length). Where it does not, or where a size exceeds 32 bits,
the header keeps the streaming form's values. Sizes are never reduced
modulo 2³¹ or 2³².

**Continuing sink.** Refused (§2).

**Preambles.** None.

## 7. Metadata

- Initial metadata: in the `LIST INFO` chunk, the exported keys mapped as in
  [wav.md §7](wav.md#7-metadata), on a file sink only
  ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 7).
  `ISFT` is written on every sink. A key with no mapping is not written.
- In-band offer: nothing.

## 8. Delay and padding

0 and 0, as [wav.md §8](wav.md#8-delay-and-padding).

## 9. End

`end(clean)` drains the rate converter and writes its samples as a last
`01wb` chunk, writes the final sizes on a seekable sink where it does (§6),
then `end`.
`end(abandoned)` writes nothing; an abandoned file keeps its streaming-form
header and is read sequentially.

## 10. Properties

- `bitrate`: from creation, `8 × picture size × framerate + 16 × channels ×
samplerate`.
- `delay`, `padding`: 0, from creation.

## 11. Live latency

The writer holds only what the converter holds; each call writes its chunks.
The bound is met by construction.

## 12. Segmenting face

Nothing. `%avi` is not segmentable (§2).

## 13. Delegated destination

Nothing.

## 14. Concurrency

No work of its own. An image scaler, when needed, belongs to one writer.

## 15. Faults

| Fault                                            | Class                                                                     |
| ------------------------------------------------ | ------------------------------------------------------------------------- |
| a parameter rejected (§1)                        | configuration error                                                       |
| a deferred size, rate or frame rate out of range | configuration error, at start-up                                          |
| a continuing sink                                | configuration error, at evaluation (§2)                                   |
| a seek failing at a clean end                    | not escalated ([encoder.md §3.6](encoder.md#36-draining-and-final-bytes)) |

No image size and no piece is refused while streaming (§4).

## 16. Tests

- With `frame.duration` giving 0, 1 and 2 images per frame: one `00db` chunk
  per image, each of the picture's size; a reader counts as many video frames
  as images.
- `%avi(width=33, height=17)`: every `00db` payload and the header's image
  size equal 33·17 + 2·17·9; a decoder reads the pictures.
- Images larger, smaller and with padded strides: the decoded picture equals
  the scaled input.
- On a regular file after a clean stop: no `idx1` chunk; the RIFF and
  `movi` sizes, total frames and stream lengths either match the content or
  hold the streaming form's values, never anything else; a reader decodes
  every chunk. On a FIFO: the streaming form, and a reader decodes every
  chunk.
- The file is read back by Liquidsoap's own AVI input and by an independent
  decoder.
- `%avi(mono)` types and builds mono.
- `output.file(append=true, %avi, …)` is refused at evaluation.
- A shared mount: a reader starting mid-stream is sent the header, then
  whole chunks, and decodes.
