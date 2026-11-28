# `%wav`

Part A. The WAV writer: a RIFF/WAVE stream of interleaved little-endian
integer PCM, with one stream and one header. This file owns `%wav`'s
parameter specification, the WAV byte layout (header, file form, streaming
form) and what the writer does with each capability. It answers every
section of [encoder.md §10](encoder.md#10-implementation-files) under the
same number.

It relies on: the seam ([encoder.md](encoder.md)); format values and their
phases ([formats.md](formats.md)); sinks ([sink.md](sink.md)); draining
([draining.md](../../source/spec/draining.md)); the fault classes
([faults.md](../../../spec/faults.md)).

The WAV streaming form of §6 is also what [`%external`](external.md) sends to
its process. Any other component that sends WAV to a process uses that form
and links here.

## 1. Format and options

`%wav(...)`. Parameters, folded in order, last one wins
([formats.md §3](formats.md#3-the-parameter-specification)):

| Parameter                                            | Value                     | Default                            | Effect                                                              |
| ---------------------------------------------------- | ------------------------- | ---------------------------------- | ------------------------------------------------------------------- |
| `mono`, `stereo`, `mono=b`, `stereo=b`, `channels=n` | literal                   | 2 channels                         | channel count, type-deciding ([formats.md §4](formats.md#4-typing)) |
| `samplerate=n`                                       | int, 1 to 2³²−1           | `frame.audio.samplerate`, deferred | sample rate of the stream                                           |
| `samplesize=n`                                       | int, one of 8, 16, 24, 32 | 16                                 | bits per sample                                                     |
| `header=b`                                           | bool                      | `true`                             | `false`: raw PCM, no RIFF header (§5)                               |
| `duration=d`                                         | number                    | —                                  | deprecated: none ([formats.md §2](formats.md#2-names-and-kinds))    |

- The type is `format(audio=pcm(<layout>))`: one audio track, no other.
- The byte rate `channels × samplerate × samplesize / 8` MUST fit in 32 bits.
  With literal values, a byte rate that does not fit is an error at type
  checking; with a computed or deferred rate, at the phase that resolves it
  ([formats.md §7](formats.md#7-when-a-format-is-validated)). The error names
  `samplerate` and states the largest rate accepted for the channel count and
  sample size.
- A `samplesize` outside the set is an error naming the value and the four
  accepted ones. Any other parameter is rejected
  ([formats.md §11](formats.md#11-errors)).
- **No adaptable parameter.** Nothing in `%wav` depends on the destination
  other than the header form and the tags, neither of which is a parameter
  (§6, §7).
- Helpers ([formats.md §9](formats.md#9-helpers)): extension `wav`, MIME
  `audio/wav`; `codecs` `1` (the WAVE format tag of integer PCM, RFC 2361);
  `bitrate` is `channels × samplerate × samplesize` when `samplerate` is
  explicit, none while it is deferred.

`duration` is deprecated: accepted with a warning, it changes no byte.
Migration: remove it. A file written by `output.file` gets exact sizes at
its clean end (§6); a stream declares an unknown length. Reason: a declared
length that nothing enforces makes readers stop early or report
corruption.

## 2. Declarations

| Declaration        | Value                                                    |
| ------------------ | -------------------------------------------------------- |
| streamable         | yes; every sample block boundary is an access point (§5) |
| needs seekable     | no                                                       |
| can continue       | yes: samples only (§6)                                   |
| in-band metadata   | no                                                       |
| segmentable        | no                                                       |
| needs no byte sink | no                                                       |
| accepts delegated  | no                                                       |

No parameter is adapted and no explicit parameter is refused against a
capability: `%wav` has no preamble and no destination-dependent option. The
header form (§6) and the `LIST`/`INFO` tags (§7) follow the capabilities.

## 3. Creation

- Inputs used: the configuration, the capabilities, the sink, the output's
  name (logs), the initial metadata (§7).
- The header form is chosen from the capabilities (§6). Nothing is started.
  Creation does not fail: a deferred value found invalid is reported before
  creation ([formats.md §6](formats.md#6-deferred-values)).
- When the configuration's rate differs from the frame's audio rate, a
  sample-rate converter is created for the channel count.

## 4. Input

- The writer reads the piece's audio track only.
- Samples are converted to the configured rate by the converter, a stage of
  [draining.md §1](../../source/spec/draining.md#1-stages).
- Each sample is converted to an integer of the sample size as
  [encoder.md §3.3](encoder.md#33-frames-in) states: 8 bits unsigned with
  silence at 128, 16, 24 (3 bytes) and 32 bits signed two's complement,
  little endian.
- Channels are interleaved in the order of the channel layout, one sample
  block (`channels × samplesize / 8` bytes) per sample period.
- No result depends on piece boundaries
  ([encoder.md §3.3](encoder.md#33-frames-in)): without rate conversion,
  each piece's samples are written whole; with it, the converter's output
  does not depend on how its input was cut.

## 5. Bytes and markers

The stream is the header (§6), then the `data` chunk's payload: the sample
blocks of every piece, in order.

- `stream_start(header_length, 0)` precedes the header. `header_length` is
  the header's length (§6), 0 when `header=false`.
- An `access_point(t)` precedes the samples of each piece, at a sample block
  boundary. The entry header is the stream's header in its streaming form.
  Reason: PCM has no framing beyond the block, so a reader starting at any
  block decodes from there given the header.
- With `header=false`, the stream is raw PCM: no byte other than sample
  blocks. A reader must be told the layout out of band.

## 6. Header forms and preambles

**Header.** The canonical header is 44 bytes, little endian:

| Offset | Size | Value                                             |
| ------ | ---- | ------------------------------------------------- |
| 0      | 4    | `RIFF`                                            |
| 4      | 4    | RIFF size: total length − 8                       |
| 8      | 4    | `WAVE`                                            |
| 12     | 8    | `fmt `, then its length 16                        |
| 20     | 2    | 1 (integer PCM)                                   |
| 22     | 2    | channels                                          |
| 24     | 4    | samplerate                                        |
| 28     | 4    | byte rate: channels × samplerate × samplesize / 8 |
| 32     | 2    | block align: channels × samplesize / 8            |
| 34     | 2    | samplesize                                        |
| 36     | 4    | `data`                                            |
| 40     | 4    | data size: the payload's length in bytes          |

- With more than 2 channels or more than 16 bits per sample, the `fmt `
  chunk MUST be `WAVE_FORMAT_EXTENSIBLE` (format tag 0xFFFE, 40 bytes: valid
  bits equal to `samplesize`, the channel mask of the layout, the PCM
  sub-format GUID), and the header grows accordingly. Reason: readers derive
  the speaker positions of more than two channels from the mask, and the
  plain tag is defined for at most two channels of at most 16 bits.
- On a file sink, the header carries the initial metadata (§7) in a
  `LIST`/`INFO` chunk between `fmt ` and `data`.
- Sizes are unsigned 32-bit values, never reduced modulo 2³¹ or 2³².

**Streaming form** (a fresh sink that is not seekable, and the first write
on a seekable one): the RIFF size and data size hold the values readers take
as "to the end of the stream": RIFF size 0xFFFFFFFF, data size
0xFFFFFFFF − (header length − 8), so that the two stay consistent.

**File form** (fresh seekable sink): the writer writes the streaming form,
then at a clean end seeks back and writes the RIFF and data sizes of what it
wrote. A data payload of odd length is followed by one pad byte, counted in
the RIFF size and not in the data size. When the payload exceeds what the
data size field holds, the header keeps the streaming form's values. A writer
MAY instead reserve a 28-byte `JUNK` chunk after `WAVE` on a seekable sink,
and turn the stream into RF64 (EBU Tech 3306) at a clean end when the
payload exceeds 32 bits.

**Continuing sink.** The writer writes sample blocks only, no header. At a
clean end on a seekable sink it reads the existing header: if it is a WAV
header of the same channels, rate and sample size, it rewrites the RIFF and
data sizes to cover the whole file; otherwise it leaves it unchanged, logged
at `severe` with the file's name
([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)). With
`header=false`, nothing is read or rewritten.

**Preambles.** None.

## 7. Metadata

- Initial metadata: written as tags under
  [encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 7: in
  the `LIST`/`INFO` chunk on a file sink, with the exported keys mapped to
  `INAM` (title), `IART` (artist), `IPRD` (album), `ICMT` (comment), `ICRD`
  (date), `IGNR` (genre); `ISFT` names the software and its version
  (item 8). Values in UTF-8, each null-terminated and padded to an even
  length. Other sinks get no `LIST` chunk, and `%wav` has no parameter that
  asks for one. Reason: a reader of a stream (a process, a player) commonly
  expects `data` to follow `fmt `.
- In-band offer: nothing.

## 8. Delay and padding

Delay and padding are 0: PCM has neither. The rate converter's latency is
compensated by draining it (§9), so the stream holds the input's duration at
the configured rate, to the nearest sample.

## 9. End

`end(clean)`:

1. drains the rate converter, if any, and writes the samples it produces;
2. on a seekable sink, writes the final sizes (§6), or on a continuing sink
   updates the existing header;
3. writes `end`.

`end(abandoned)` releases the converter and writes nothing. A file abandoned
part way keeps its streaming-form header, which stays valid.

## 10. Properties

- `bitrate`: `channels × samplerate × samplesize`, from creation.
- `delay`, `padding`: 0, from creation.

## 11. Live latency

The writer holds only what the rate converter holds; every call writes the
samples it produced. The bound is met by construction.

## 12. Segmenting face

Nothing. `%wav` is not segmentable (§2).

## 13. Delegated destination

Nothing.

## 14. Concurrency

No work of its own and no state shared with other writers.

## 15. Faults

| Fault                                                         | Class                                                                     |
| ------------------------------------------------------------- | ------------------------------------------------------------------------- |
| a parameter rejected (§1)                                     | configuration error                                                       |
| a deferred rate out of range, or whose byte rate does not fit | configuration error, at start-up                                          |
| a continuing sink's header not of this configuration          | not escalated: logged at `severe` (§6)                                    |
| a seek or a read back failing at a clean end                  | not escalated ([encoder.md §3.6](encoder.md#36-draining-and-final-bytes)) |

The writer reports no item fault: every sample has an encoding (§4).

## 16. Tests

- Header bytes for 1 and 2 channels × each sample size, on a FIFO and on a
  regular file: the streaming form's values on the FIFO; on the file after a
  clean stop, RIFF and data sizes equal to the payload, and a decoder reads
  exactly the samples written.
- 6 channels, and 2 channels at 24 bits: the `fmt ` chunk is extensible with
  the layout's mask; a decoder reports the layout.
- A ramp cut into pieces of random lengths decodes bit-exact against the
  same ramp in one piece; with a converter, sample-count exact.
- A clean stop with `samplerate` different from the frame rate: the decoded
  length equals the input's duration to within one sample.
- An abandoned file: its header holds the streaming form, and it decodes.
- Append one minute to a one-minute file of the same configuration: one
  header, sizes covering two minutes. Append to a file of another rate: the
  header is unchanged and a `severe` line names the file.
- A shared mount: a reader starting mid-stream is sent the header and then
  whole sample blocks, and decodes.
- `output.file` with metadata: the `INFO` chunk holds the title; a FIFO gets
  none.
- `%wav(duration=1.)` is accepted, logs one deprecation warning, and gives
  the same bytes as `%wav`; `%wav(samplesize=12)` fails `--check` with a
  position; a computed rate whose byte rate overflows fails at evaluation.
- A payload past 4 GiB (with a sink double): no size is ever wrapped.
