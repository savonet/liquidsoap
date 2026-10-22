# Inline encoding and decoding operators (Part B, OCaml)

Advice for implementing [inline-codecs.md](../inline-codecs.md) in OCaml on
top of the FFmpeg bindings. Part A is complete without it.

## 1. Registration

The eight track operators are track operators of the Lang API, under the
modules `track.ffmpeg.encode`, `track.ffmpeg.raw.encode`,
`track.ffmpeg.decode` and `track.ffmpeg.raw.decode`. Each returns the input
track's field and the producer source. Building all eight from one function
over ``[ `Audio_encoded | `Audio_raw | `Video_encoded | `Video_raw ]`` keeps
the typing rules of Part A §1 in one place.

The encoders' format argument is a format type over a frame type whose row
is open (`Lang.univ_t ()` as the base) and holds only `audio` or `video`.
The open row is what lets one format serve both halves of an `audio_video`
wrapper.

The twelve source wrappers are script functions in the FFmpeg standard
library file, guarded by `%ifdef` on the track operators.

## 2. Producer

The clock spec asks for one reusable child-clock reader component
([clock spec §15](../../../../clock/spec/clock.md#15-child-clocks)). Build
the producer on it and declare only what is the producer's own: shared
reader, the buffer limit, no self-sync refusal, and the transform hooks
(convert a frame, drain, discard).

Keep the transform state as one variant:

```ocaml
type 'transform state = [ `Idle | `Running of 'transform ]
```

The reading step of Part A §3.4 matches on it: input not ready and
`` `Running t `` drains `t`, writes the result, and moves to `` `Idle ``.

The placement of metadata and track marks (Part A §3.3) is a queue of
`(input_time, entry)` owned by the producer. The transform hands each output
item to a `put ~input_time item` function; `put` first writes every queued
entry whose input time is at or before `input_time`, then the item. Ending a
transform writes what is left in the queue. Encoders compute `input_time`
from the packet or frame pts with the time-base conversion of common.md.
Decoders keep a small map from input pts to input time, filled when a packet
is sent and read when a frame comes out (`Avutil.Frame.pts`); entries older
than the last frame out can go.

## 3. Source-id records

Use a table keyed by the input source's unique id string (a source keeps its
id for its lifetime), with an explicit count of live transforms. Releasing a
transform decrements the count and removes the entry at 0. This gives the
exact lifetime of Part A §5 with no dependence on the garbage collector and
no `Weak` table.

```ocaml
type record = {
  id : int64;
  mutable elapsed : int;
  mutable is_open : bool;
  mutable users : int;
}
```

`elapsed` is in main ticks. Advance it once per child-clock tick, from the
frame the child output receives, not once per producer: producers sharing
the child clock see the same frame.

## 4. Encoder transform

Bindings used (see the bindings spec):

- audio: `Avcodec.Audio.find_encoder_by_name`,
  `Avcodec.Audio.find_best_sample_format`, `Avcodec.Audio.create_encoder`
  with the options table, `Avcodec.capabilities` for the variable-frame-size
  capability, `Avcodec.Audio.frame_size`, `Avcodec.encode`,
  `Avcodec.flush_encoder`; `Swresample` from planar float arrays to frames;
- video: `Swscale` from the packed YUV planes to frames, the Fps helper of
  filters.md built on `Avfilter`, `Avcodec.Video.create_encoder` with the
  hardware context of common.md;
- `Avcodec.encode` delivers packets with timestamps in the encoder's time
  base; `Avcodec.flush_encoder` signals end of stream and delivers the rest
  (avcodec.md §4.10).

Codec options: pass a fresh copy of the stream's option table to each
encoder creation. The bindings remove the options they consume; what is left
is the "unrecognized options" error of Part A §4.

The evaluation-time checks of Part A §4 (codec present, encoder found, raw
options) need only the parsed format and `find_encoder_by_name`; no encoder
is opened.

## 5. Chunker

Use the duration helper of common.md as specified there (lengths from
absolute timestamps), and call its flush when the transform drains.

## 6. Decoder transforms

- Audio: `Avcodec.Audio.find_decoder`, `Avcodec.Audio.create_decoder` with
  the content's codec parameters, `Avcodec.decode`, `Avcodec.flush_decoder`,
  and `Swresample` from frames to planar float arrays. The output channel
  layout comes from the producer's resolved content type, read at the first
  build.
- Video: decoder as above, the display stage of side-data.md, the internal
  video converter of decoder-internal.md.
- Compare decoder parameters by value (codec id, extradata bytes and the
  fields content.md compares), never by physical identity of the parameter
  record: identical parameters may arrive in distinct records.
- Raw parameters are optional fields of the content parameters; match on
  them, and on a missing one drop the run as an item fault with a warning
  naming it.
- Never raise from the transform for a data fault: return a result the
  producer turns into an item fault or a source fault (Part A §15). The one
  raise is the hard error of an infallible producer whose source fault
  persists.

## 7. Tests

What the test suite pins for these operators is in
[tests.md](../tests.md); the conformance items of Part A add to it.
