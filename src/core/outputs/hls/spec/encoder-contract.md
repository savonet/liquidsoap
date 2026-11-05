# The encoder seam

Part A. The HLS output drives any registered encoder through one interface.
This file owns that interface: the calls, their order, and what each answer
must satisfy. Each implementation has its own file, with the same section
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
   with the HLS flag and the variant name. An encoder is never reused
   across runs.
2. `init` once, before the first chunk.
3. Per chunk: `init_encode` until it returns `Init`, then `split_encode`.
4. At every segment open: `insert_id3` when the variant's ID3 mode is
   prepended, then `header()`. Both also before the first `init_encode`,
   since the first segment opens first. `insert_id3` for in-stream ID3 at
   the times of [metadata.md §2](metadata.md#2-id3).
5. `stop()` once, at the end of a run that ends on a `stop`.
6. Properties (§6) after `init_encode` has returned `Init`.

Calls to one encoder are serialised. The output never uses the encoder's
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

`split_encode ~boundary ~force chunk → piece list`, in byte order:

- `Bytes b`: output bytes.
- `Split {time, forced}`: a position where a segment may start; `time` is
  the media time of what follows. Every stream's first packet after it is a
  key packet, unless `forced`.
- `Restart {time, init}`: a split point that also starts a new container;
  `init` its init segment or none.

`boundary` is the request ([segmenting.md §2](segmenting.md#2-segments-in-memory)).
An encoder that makes key frames makes one at or just after each request,
and an encoder whose streams are all-key has a split point within one codec
frame after it.

`force` true asks for a split at the start of this chunk without the
encoder's condition ([segmenting.md §5](segmenting.md#5-the-target-duration-bound)).
An encoder MAY decline it only while a split point it has committed to is
on its way: a key frame it made for a request, or a restart not yet
returned.

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
| `independent_segments ()` | true when the encoder's split points, not counting forced ones, start with key packets   |
| `free_split ()`           | true when a split point is within one codec frame of every request                       |
| `bitrate ()`              | nominal bits per second, or none                                                         |
| `video_size ()`           | `(width, height)`, or none                                                               |

Answers are valid from the `Init` of §4 and may change at a `Restart`. None
defers to the format and the variant fields
([variants.md §3](variants.md#3-derived-properties)). Whether a listed
segment started at a forced split is recorded by the output
([segmenting.md §2](segmenting.md#2-segments-in-memory)), not answered by
the encoder.

## 7. `insert_id3`

`insert_id3 ~time metadata → bytes option`. `time` is media time in ticks.
For a prepended mode, the result is the tag to start the segment, including
its timestamp; for in-stream, the encoder writes a timed tag at `time` and
returns none.

## 8. Header and stop

`header ()`: bytes written at the start of every segment, after any
prepended ID3 tag. An encoder whose header is known only after its first
chunk puts it at the start of that chunk's bytes. `stop ()`: final bytes,
appended to the open segment; they carry no split point.

## 9. Conformance

[conformance.md §4](conformance.md#4-encoder-seam).
