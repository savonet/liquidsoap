# The encoder seam

Part A. The HLS output drives any registered encoder through one interface,
the segmenting face of the encoder spec
([encoder.md §4](../../../encoder/spec/encoder.md#4-the-segmenting-face)).
This file owns that interface: the calls, their order, and what each answer
must satisfy. The registry and the rules every instance follows are
[encoder.md §1–§2](../../../encoder/spec/encoder.md#2-instances). Each implementation has its own file, with the same section
numbers:

| File                                               | Encoders                    |
| -------------------------------------------------- | --------------------------- |
| [encoder-packed-audio.md](encoder-packed-audio.md) | `%mp3`, `%shine`, `%fdkaac` |
| [encoder-ffmpeg.md](encoder-ffmpeg.md)             | `%ffmpeg`                   |

Each implementation file answers every section; a missing answer is a gap.

## 1. Role

The encoder turns chunks into bytes and says where those bytes may be cut.
The output owns the grid, the cuts, files and playlists
([segmenting.md](segmenting.md)); the encoder never sees a segment.

Times are media time of the run, in main ticks
([segmenting.md §1](segmenting.md#1-time-the-grid-and-the-target-duration)).
The media time an encoder counts from the chunks it receives MUST equal the
output's.

## 2. Calls and order

1. A new encoder is created for each variant at the start of every run,
   with the HLS flag, the variant name and the segment duration `D` in
   seconds ([segmenting.md §1](segmenting.md#1-time-the-grid-and-the-target-duration),
   [encoder.md §2](../../../encoder/spec/encoder.md#2-instances)).
2. `init` once, before the first chunk.
3. Per chunk: `init_encode` until it returns `Init`, then `split_encode`.
4. Until the run's offset is set, after each chunk: `initial_delay ()` on
   every variant. It answers how far before media time 0 the variant's
   first timestamp lies, the lowest first DTS of its audio and video
   streams, or none while one of them has produced no packet. Until the
   offset is set the encoder holds its output: `init_encode` returns
   `Pending`. Once every variant answers, or after 10 s of media time or at
   a stop with the answers known so far (none counting as 0, and logged),
   `set_offset` with the largest answer on every variant, and each
   variant's first segment opens at media time 0. Every timestamp an
   encoder writes, ID3 timestamps included, is media time plus the offset.
   Reason: one offset for all streams and variants keeps their timestamps
   matching (lip-sync, and RFC 8216 §6.2.4 across variants), and none is
   negative, which MPEG-TS cannot store: its 33-bit timestamps wrap. The
   first DTS is measured, not read from the codec: an encoder may report a
   reordering delay below its real one.
5. At every segment open: `insert_id3` when the variant's ID3 mode is
   prepended, also before the first `init_encode`, since the first segment
   opens first. `insert_id3` for in-stream ID3 at
   the times of [metadata.md §2](metadata.md#2-id3).
6. `stop()` once, at the end of a run that ends on a `stop`.
7. Properties (§6) after `init_encode` has returned `Init`.

Every instance serialises its own calls
([encoder.md §2](../../../encoder/spec/encoder.md#2-instances)). The output never uses the encoder's
in-stream metadata entry or the plain `encode` other outputs use.

## 3. `init`

`init ?id3 ?id3_version () → id3_mode`, with `id3_mode` one of `none`,
`in_stream`, `prepended` ([metadata.md §2](metadata.md#2-id3)). `id3` is
the variant's field when given.

- `id3 = true` on an encoder that can carry no ID3 MUST be rejected with an
  invalid-value error naming the variant: the script asked for something
  it will not get.
- `id3 = false` on an encoder whose format requires ID3 (packed audio) MUST
  be rejected likewise.
- Otherwise the answer is the encoder's: its default when `id3` is not
  given.

Errors here are construction errors ([operator.md §2](operator.md#2-construction)).

## 4. `init_encode`

`init_encode ~boundary chunk → Pending | Init (init, pieces)`. `boundary`
is the request of [segmenting.md §2](segmenting.md#2-segments-in-memory).
`Pending`: the chunk is accepted, its bytes held by the encoder. `Init`:
`init` is the init segment, none for a format without one; `pieces` hold
every byte after it, as in §5. No byte of a held chunk is lost. An encoder
whose format has a header MUST NOT return empty init bytes.

## 5. `split_encode`

`split_encode ~boundary chunk → piece list`, in byte order:

- `Bytes b`: output bytes.
- `Split {time}`: a position where a segment may start; `time` is the media
  time of what follows. Every stream's first packet after it is a key
  packet.
- `Restart {time, init}`: a split point that also starts a new container;
  `init` its init segment or none.

`boundary` is the request ([segmenting.md §2](segmenting.md#2-segments-in-memory)).
An encoder whose streams are all-key has a split point within one codec
frame after it. An encoder with key frames has a split point at each of
them and does not place them by the request.

An encoder that encodes video SHOULD default its key-frame interval from
`D` (§2) when the script sets none, so that key frames divide `D` and
fall at most every 2 s. Reason: the output cuts key-frame variants only at
key frames ([segmenting.md §4.1](segmenting.md#41-key-frame-variants)), and
an encoder's own default (libx264: 250 frames, plus scene cuts) gives
segments of irregular length.

The output relies on:

- split times strictly increasing, and each at or before the media time
  reached after the chunk;
- the bytes after a split point, preceded by the init segment if any,
  decoding on their own: an MPEG-TS segment starts with a PAT and a PMT
  (RFC 8216 §3.2);
- the pieces of one chunk returned in one call.

## 6. Properties

| Query                     | Meaning for the output                                                                   |
| ------------------------- | ---------------------------------------------------------------------------------------- |
| `codec_attrs ()`          | RFC 6381 codec string listing every format in the variant, or none; never a partial list |
| `independent_segments ()` | true when the encoder's split points start with key packets                              |
| `free_split ()`           | true when a split point is within one codec frame of every request                       |
| `bitrate ()`              | nominal bits per second, or none                                                         |
| `video_size ()`           | `(width, height)`, or none                                                               |

Answers are valid from the `Init` of §4 and may change later, at a
`Restart` or once a stream's parameters are known (a copied stream that
waits for its source); the output reads them again at every segment
close. None
defers to the format and the variant fields
([variants.md §3](variants.md#3-derived-properties)).

## 7. `insert_id3`

`insert_id3 ~time metadata → bytes option`. `time` is media time in ticks.
For a prepended mode, the result is the tag to start the segment, including
its timestamp; for in-stream, the encoder writes a timed tag at `time` and
returns none.

## 8. Stop

`stop () → Init (init, pieces)`: the final pieces, as §5, applied to the
open segment before it closes. `init` is as in §4 when `init_encode` has
not yet returned `Init`: an encoder stopped within its first segment can
learn its init segment only at its stop. It is none otherwise. A format's
header lives in its init segment; no bytes are repeated at the start of each
segment. Reason: an
encoder that holds media (lookahead, reordering) returns at its stop the
split points of the media it held, and dropping them would make the last
segment exceed the target duration.

## 9. Conformance

[conformance.md §4](conformance.md#4-encoder-seam).
