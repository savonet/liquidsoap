# The encoder seam

Part A. The HLS output drives, per variant, one
[container writer](../../../encoder/spec/encoder.md#3-the-stream-face) built
with a `segmented(D)` sink: the segmenting face of the encoder spec
([encoder.md §4](../../../encoder/spec/encoder.md#4-the-segmenting-face)),
which maps this file's items onto the writer's bytes and markers. This file
owns what that face adds for HLS: the calls and their order, the split
requests, the ID3 mode and `insert_id3`, the per-variant properties, and
what the output relies on at its cut points. The registry, a writer's
lifetime, its markers, its ends and its faults are
[encoder.md](../../../encoder/spec/encoder.md); the segmented sink's
capability is [sink.md §1](../../../encoder/spec/sink.md#1-capabilities).
Each implementation has its own file, with the same section numbers:

| File                                               | Writers                     |
| -------------------------------------------------- | --------------------------- |
| [encoder-packed-audio.md](encoder-packed-audio.md) | `%mp3`, `%shine`, `%fdkaac` |
| [encoder-ffmpeg.md](encoder-ffmpeg.md)             | `%ffmpeg`                   |

Each implementation file answers every section; a missing answer is a gap.

## 1. Role

The writer turns chunks into bytes and marks where those bytes may be cut.
The output owns the grid, the cuts, files and playlists
([segmenting.md](segmenting.md)); the writer never sees a segment.

Times are media time of the run, in main ticks
([segmenting.md §1](segmenting.md#1-time-the-grid-and-the-target-duration)).
The media time a writer counts from the chunks it receives MUST equal the
output's ([encoder.md §3.3](../../../encoder/spec/encoder.md#33-frames-in)).

In this tree's terms, the writer's items are:

| This tree             | Writer                                                                                      |
| --------------------- | ------------------------------------------------------------------------------------------- |
| init segment          | the header of a stream start                                                                |
| split point at `time` | `access_point(time)`, and the stream start that begins each stream                          |
| restart               | a later `stream_start(header_length, time)`: a split point that also starts a new container |
| pieces                | the bytes and markers a call writes into the sink, in order                                 |

## 2. Calls and order

1. The output declares, per variant, a `segmented(D)` sink, with `D` the
   segment duration in seconds
   ([segmenting.md §1](segmenting.md#1-time-the-grid-and-the-target-duration)).
   At construction it reads the configuration's declarations (segmentable,
   the ID3 mode of §3) without building a writer
   ([encoder.md §1](../../../encoder/spec/encoder.md#1-registry), item 4).
   Reading the ID3 mode is not a call on a writer: it is a function of the
   configuration, needed at evaluation, so the writer's calls are items 2–6
   only.
2. A new writer is built for each variant at the start of every run, with
   the variant name for its logs
   ([encoder.md §2.1](../../../encoder/spec/encoder.md#21-one-writer-per-destination-stream)).
3. Per chunk: `write(chunk)`. Before each `write`, the sink holds the
   chunk's **split request**, the media time of
   [segmenting.md §2](segmenting.md#2-segments-in-memory); the writer reads
   it from its sink while it writes that chunk.
4. At every segment open: `insert_id3` when the variant's ID3 mode is
   prepended, the first segment included, before the first `write`.
   `insert_id3` for in-stream ID3 at the times of
   [metadata.md §2](metadata.md#2-id3).
5. `end(clean)` once, at the end of a run that ends on a stop (§8);
   `end(abandoned)` once, at the end of a run that ends on an error
   ([operator.md §6](operator.md#6-errors-and-reopening)).
6. Properties (§6) once the writer's first stream start has been written.

One caller makes every call: the output's streaming work
([encoder.md §7](../../../encoder/spec/encoder.md#7-concurrency)). The
output never offers a writer in-band metadata
([encoder.md §9](../../../encoder/spec/encoder.md#9-in-band-metadata)).

## 3. ID3 mode

Each configuration declares, for a `segmented` sink, its ID3 mode, one of
`none`, `in_stream`, `prepended` ([metadata.md §2](metadata.md#2-id3)), as a
function of the variant's `id3` field, without building a writer. The field
is checked at construction:

- `id3 = true` on a configuration whose mode is `none` MUST be rejected
  with an invalid-value error naming the variant: the script asked for
  something it will not get.
- `id3 = false` on a configuration whose format requires ID3 (packed
  audio, mode `prepended`) MUST be rejected likewise.
- Otherwise the mode is the one the configuration declares for that `id3`
  field, its default when the field is not given.

The variant's `id3_version` is handed to the writer at creation. Errors here
are construction errors ([operator.md §2](operator.md#2-construction)).

## 4. The first stream start

Nothing is written before the writer's first stream start, and a stream
start is written with its header's bytes
([encoder.md §3.4](../../../encoder/spec/encoder.md#34-bytes-and-markers-out),
items 1 and 3): chunks the writer accepted while it could not yet know its
header are held, and none of their bytes is lost. The first stream start's
header is the variant's init segment, none for a format without one
(`header_length` 0). A writer whose format has a header MUST NOT write a
first stream start with an empty header.

## 5. Access points and restarts

After its first stream start, a writer writes bytes, access points and
restarts (§1) in byte order. At an access point every stream's next packet
is a key packet. A restart's header is its init segment, or none.

A writer whose streams are all-key marks an access point within one codec
frame after each split request. A writer with key frames marks one at each
of them and does not place them by the request.

A writer that encodes video SHOULD default its key-frame interval from `D`
(§2) when the script sets none, so that key frames divide `D` and fall at
most every 2 s. Reason: the output cuts key-frame variants only at key
frames ([segmenting.md §4.1](segmenting.md#41-key-frame-variants)), and a
codec's own default (libx264: 250 frames, plus scene cuts) gives segments of
irregular length.

The output relies on:

- access point times strictly increasing, and each at or before the media
  time reached after the chunk;
- the bytes after an access point, preceded by the init segment if any,
  decoding on their own: an MPEG-TS segment starts with a PAT and a PMT
  (RFC 8216 §3.2);
- everything a chunk produces being in the sink when its `write` returns
  ([encoder.md §3.2](../../../encoder/spec/encoder.md#32-order-of-a-calls-output)).

## 6. Properties

| Query                     | Meaning for the output                                                                   |
| ------------------------- | ---------------------------------------------------------------------------------------- |
| `codec_attrs ()`          | RFC 6381 codec string listing every format in the variant, or none; never a partial list |
| `independent_segments ()` | true when the writer's access points start with key packets                              |
| `free_split ()`           | true when an access point is within one codec frame of every split request               |
| `bitrate`                 | [encoder.md §3.8](../../../encoder/spec/encoder.md#38-bitrate)                           |
| `video_size ()`           | `(width, height)`, or none                                                               |

Answers are valid from the first stream start (§4) and may change later, at
a restart or once a stream's parameters are known (a copied stream that
waits for its source); the output reads them again at every segment close.
None defers to the format and the variant fields
([variants.md §3](variants.md#3-derived-properties)).

## 7. `insert_id3`

`insert_id3 ~time metadata → bytes option`. `time` is media time in ticks.
For a prepended mode, the result is the tag to start the segment, including
its timestamp; for in-stream, the writer writes a timed tag at `time` into
its sink and returns none. It is an operation of this contract, not the
in-band metadata of
[encoder.md §9](../../../encoder/spec/encoder.md#9-in-band-metadata).

## 8. Stop

At a stop, `end(clean)`
([encoder.md §3.6](../../../encoder/spec/encoder.md#36-draining-and-final-bytes)):
its final bytes, and the access points among them, are applied to the open
segment before it closes. A writer ended within its first segment can learn
its header only at its end: its first stream start, and with it the init
segment, then comes with the final bytes. A format's header lives in its
init segment; no bytes are repeated at the start of each segment. Reason: a
writer that holds media (lookahead, reordering) marks at its end the access
points of the media it held, and dropping them would make the last segment
exceed the target duration.

At an error, `end(abandoned)`: the writer writes nothing more, and its held
bytes are lost with the aborted segment
([operator.md §6](operator.md#6-errors-and-reopening)).

## 9. Conformance

[conformance.md §4](conformance.md#4-encoder-seam).
