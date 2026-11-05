# The encoder seam

Part A. As built. The HLS output drives any registered encoder through one
interface. This file owns that interface: the calls, their order, and what
the output expects of each answer. Each implementation has its own file,
with the same section numbers:

| File                                               | Encoders                                             |
| -------------------------------------------------- | ---------------------------------------------------- |
| [encoder-frame-units.md](encoder-frame-units.md)   | `%wav`, `%avi`, `%flac`, `%ogg`, `%external`, `%ndi` |
| [encoder-packed-audio.md](encoder-packed-audio.md) | `%mp3`, `%shine`, `%fdkaac`                          |
| [encoder-ffmpeg.md](encoder-ffmpeg.md)             | `%ffmpeg`                                            |

## 1. Role

The encoder turns chunks into bytes and tells the output where those bytes
may be cut. The output owns the grid, the cuts, files and playlists. The
encoder never sees a segment.

Times are media time in main ticks
([segmenting.md §1](segmenting.md#1-segment-duration-and-the-grid)). An
encoder counts its own media time from the chunks it receives.

## 2. Calls and order

1. The encoder is created by its factory with the HLS flag, the variant
   name and empty metadata.
2. `init` once, at construction.
3. Per chunk: `init_encode` until it returns `Init`, then `split_encode`
   for every later chunk.
4. `header()` at every segment open, before any other byte of that
   segment; `insert_id3` at every segment open when ID3 is enabled, before
   `header()`. Both happen before the first `init_encode` too, since the
   first segment opens first.
5. `stop()` once, at the output's `stop`. Its bytes go to the open segment.
6. Properties (§6) at any time; the output reads them after the first
   segment close.

Every call to one encoder is serialised. The output never calls the
encoder's in-stream metadata entry, nor the plain `encode` used by other
outputs.

## 3. `init`

`init ?id3_enabled ?id3_version () → bool`. `id3_enabled` is the variant's
`id3` field when given; `id3_version` likewise. Returns whether segments of
this variant carry ID3. May raise to reject the combination (the script
stops).

## 4. `init_encode`

`init_encode ~boundary chunk → Pending | Init (init, pieces)`. `boundary`
is the next grid boundary. `Pending`: the chunk is accepted, its bytes kept
by the encoder for later. `Init`: `init` is the init segment (none for a
format without one; empty bytes are treated as none with a log,
[files.md §6](files.md#6-init-segments)) and `pieces` every byte after it,
as in §5.

## 5. `split_encode`

`split_encode ~boundary ~force chunk → piece list`, the pieces in byte
order:

- `Bytes b`: output bytes.
- `Split {time, forced}`: a position between the bytes before and after it
  where a segment may start; `time` is the media time of what follows.
  `forced` marks a split made because `force` was true, without the
  encoder's own condition.
- `Restart {time, init}`: a split point that also starts a new container;
  `init` its init segment or none.

`force` true asks for a split at the start of this chunk, whatever the
encoder's condition ([segmenting.md §6](segmenting.md#6-the-target-duration-bound)).
An encoder may decline when a split point is already due.

The output relies on: split times increasing; a split's time being at or
before the media time reached after the chunk; pieces of one chunk
returned in one call.

## 6. Properties

| Query                     | Meaning for the output                           |
| ------------------------- | ------------------------------------------------ |
| `codec_attrs ()`          | RFC 6381 codec string for `CODECS`, or none      |
| `independent_segments ()` | whether to announce `EXT-X-INDEPENDENT-SEGMENTS` |
| `bitrate ()`              | bits per second, or none                         |
| `video_size ()`           | `(width, height)`, or none                       |

None defers to the format and variant fields
([variants.md §3](variants.md#3-derived-properties)).

## 7. `insert_id3`

`insert_id3 ~frame_position ~sample_position metadata → bytes option`. The
position is the opening segment's start: frame number and offset in ticks
within the frame. Bytes are prepended to the segment; none means the
encoder carries the tag itself, or carries none.

## 8. Header and stop

`header ()`: bytes written at the start of every segment, after any ID3
tag. `stop ()`: final bytes, flushed into the open segment; no pieces, so
split points in them are lost.

## 9. Conformance

[conformance.md §4](conformance.md#4-encoder-seam).
