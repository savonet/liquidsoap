# Language notes: the `%ffmpeg` encoder (OCaml)

Part B for [encoder-format.md](../encoder-format.md),
[encoder.md](../encoder.md), [encoder-internal.md](../encoder-internal.md),
[encoder-copy.md](../encoder-copy.md) and [timestamps.md](../timestamps.md):
advice on building the encoder in OCaml on the FFmpeg bindings. §5 is owned
with encoder-copy.md.

## 1. Libraries and registration

- Keep the format record, its parser's pure parts, `to_string` and the
  content-type function in a library with no libav dependency, so that a
  process without FFmpeg (the JS/LSP build) type-checks `%ffmpeg` terms. Give
  it a hook for the three facts only FFmpeg knows: the media type of a codec
  name, the channel count of a layout name, and whether a pixel format has
  alpha ([encoder-format.md §10.1](../encoder-format.md#101-literal-values-at-type-checking-time)).
  The libav library installs the hook at load.
- The type checker and the instantiating output call the same content-type
  function, on the term and on the record. Write it once over a small view
  type (field, mode, media type, channel count, layout, alpha) that both the
  term and the record can produce; two functions kept consistent by hand
  drift ([encoder-format.md §10](../encoder-format.md#10-content-type-implied-by-a-format)).
- `Avutil.Options` listing (avutil.md §4.15) gives the option names of a codec
  and of the generic codec context, for the build-time check of
  [encoder.md §4](../encoder.md#4-option-handling).

## 2. Writing streams on the bindings

| Spec step                      | Bindings                                                                                               |
| ------------------------------ | ------------------------------------------------------------------------------------------------------ |
| `Stream` target                | `Av.open_output_stream` with a write closure appending to a `Buffer.t`, and a seek closure for HLS mp4 |
| `Url` target                   | `Av.open_output ?interrupt`                                                                            |
| copy stream                    | `Av.new_stream_copy` at the first frame (all streams are created then, in track order)                 |
| encoded audio, video, subtitle | `Av.new_audio_stream`, `new_video_stream`, `new_subtitle_stream` at the first frame                    |
| muxer flush at a split point   | `Av.flush`                                                                                             |
| codec drain and trailer        | `Av.close`                                                                                             |
| re-framer                      | `Avfilter.Utils.init_audio_converter ~out_frame_size`, drained with `` `Flush ``                       |
| resampler                      | `Swresample` `convert` / `flush`                                                                       |
| frame-rate converter           | the Fps helper graph, drained with `eof`                                                               |

Since every stream is created at the first frame, nothing needs
`new_uninitialized_stream_copy`.

**Per-packet hook.** `write_frame ~on_keyframe` reports key packets only,
while the split rule of [encoder.md §8.2](../encoder.md#82-keyframe-hooks)
needs every packet with its flag. Encode with the avcodec encoder
(`Avcodec.Audio.create_encoder` and the video equivalent: send frames,
receive packets) and write each packet with `write_packet` on a stream
created from the encoder's parameters; the stream encoder then sees every
packet before the muxer. This also gives the draining step explicit control
of the codec instead of leaving it to `close`.

**Key frame requests.** `request_keyframe` needs a way to mark a frame as a
key frame request (FFmpeg's `pict_type = AV_PICTURE_TYPE_I`); the bindings'
frame API needs that setter.

**Header bytes.** The init segment and `header ()` need the position where
the header ends. Record the buffer length when the header write completes:
an explicit header write in the bindings (`avformat_write_header`) gives it
for WebM and Ogg; fragmented MP4 with `delay_moov` writes `moov` at the first
fragment, so take the position `Av.tell` reports after the first flush.

## 3. Order

Feed and create stream encoders by folding over `streams` in written order
into a list, not into a map whose `add` prepends: the order is binding
(container stream order, `codec_attrs` order).

## 4. State as values

- Instance state is one variant:
  ``[ `Built of container | `Started of container | `Failed of exn | `Stopped ]``;
  a restart replaces `Started c` by `Built c'`. The serialising mutex of the
  generic layer is the only lock needed.
- The HLS split is ``[ `Idle | `Armed | `Candidate of int * held_bytes ]``,
  not a flag plus a callback reset by `Fun.protect`.
- Per video stream: ``[ `Waiting_first_frame | `Running of stream * converter ]``
  where `converter` carries its key (pixel format, time base, source id).
- Per raw audio stream: the resampler with its input key, as for video.

## 5. Per-source groups of copy tracks

Advice for [encoder-copy.md §12](../encoder-copy.md#12-source-switching-and-per-source-state)
and [timestamps.md §6](../timestamps.md#6-copy-streams-per-track).

### 5.1 One table per container, keyed by id

A plain `(int64, group) Hashtbl.t` owned by the container's handler (the value
an Ogg or HLS restart replaces) holds the groups. Dropping the handler drops
every group with it, so a restart needs no cleanup and cannot leak state into
the next container. A `Weak` table is the wrong tool: the lifetime of
encoder-copy.md §12.5 is defined by membership, which the encoder knows
exactly, and a weak table makes it depend on the GC.

```ocaml
type group = {
  mutable state : [ `Waiting of { since : int64 } | `Started of { offset : int64 option } ];
  mutable members : track list;
}
```

`offset = None` is a started group with no packet yet (sparse sources).

### 5.2 Tracks own their tenure

Each copy track keeps its current tenure as one value:

```ocaml
type tenure = {
  source : int64;
  group : group;
  mutable ready : bool;
  held : (int64 * Avcodec.Packet.t) Queue.t;  (* position, packet *)
  mutable consecutive_faults : int;
}
```

`consecutive_faults` counts item faults since the tenure last wrote a packet;
reaching `max_consecutive_faults` triggers the group restart of encoder-copy.md
§12.3. A group restart builds fresh tenures with the count at 0, so each
later run restarts the group again.

The track holds `tenure option` (`None` before its first run). A switch
closes the tenure: the track removes itself from `group.members` unless the
tenure was ready, in which case it stays listed until the end-of-frame step.

### 5.3 The end-of-frame step

`Ffmpeg_encoder` runs one function after the field loop of `encode`
([encoder.md §5](../encoder.md#5-encoding-a-frame)): for every group touched
in the frame, drop closed ready tenures from `members` after evaluating the
start, start or discard groups per §12.3, write held packets, and remove
groups with no members from the table. Keeping the set of touched groups in a
per-frame list avoids scanning the table.

### 5.4 Offsets in main ticks

Offsets and track ends are `int64` main ticks; per-packet conversions use the
rescale of [common.md §5](../common.md#5-time-base-conversion) from the
packet's absolute values. Once the header is written, `Av.get_time_base` of
the stream gives the stored time base for the check of timestamps.md §6.3;
converting there and calling `write_packet` with that time base makes the
checked value the stored one.

The output position counter and the `skipped` total of timestamps.md §6.2
live together on the timeline: on the handler for Ogg and on the instance
under HLS, because an HLS restart keeps the variant's timeline
(timestamps.md §1.4).

## 6. Tests

What the suite pins is in [tests.md](../tests.md).

## 7. External facts relied on

- libavcodec pads a short final audio frame with silence for codecs without
  the small-last-frame capability, and accepts one short frame at the end.
- The Ogg muxer writes whole pages, and its header write emits the codec
  header pages; a chained Ogg stream is a concatenation of complete Ogg
  streams (RFC 3533).
- The interleaving muxer holds packets up to `max_interleave_delta` (default
  10 s) waiting for sparse streams.
