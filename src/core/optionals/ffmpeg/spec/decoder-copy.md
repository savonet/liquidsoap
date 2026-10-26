# Copy per-stream decoders

This file owns the per-stream decoders that pass demuxed FFmpeg packets
through undecoded, in the `ffmpeg.copy` content type of
[content.md](content.md). It answers every clause of
[decoder.md, Per-stream decoder contract](decoder.md#7-per-stream-decoder-contract)
with the same section numbers. It relies on the duration helper as
[decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments) configures
it and on the subtitle timeline of
[decoder.md §16.2](decoder.md#162-subtitle-timeline). How the encoder turns
these packets' timestamps into output timestamps is in
[timestamps.md](timestamps.md).

## 1. Selection

| Decoder        | Stream            | Requested format | Sparse |
| -------------- | ----------------- | ---------------- | ------ |
| copy audio     | audio, packets    | `ffmpeg.copy`    | no     |
| copy video     | video, packets    | `ffmpeg.copy`    | no     |
| copy subtitles | subtitle, packets | `ffmpeg.copy`    | yes    |

Each is created with the stream index, the requested format, the stream, the
field and the codec parameters.

## 2. Construction

1. The copy parameters are built from the stream as
   [content.md §3.1](content.md#31-ffmpegcopy) defines them: every property a
   muxer's stream header depends on.
2. The requested format is unified with them
   ([content.md §3.1](content.md#31-ffmpegcopy)). A failure is an error and
   the decoder creation fails.
3. Audio and video: a duration helper in DTS mode on the stream time base
   ([decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments)).
4. Subtitles: a subtitle timeline
   ([decoder.md §16.2](decoder.md#162-subtitle-timeline)) whose data are
   lists of packets; merging concatenates the lists.

## 3. Input

Packets; `switch`; `Flush`. Copy subtitles also receive `advance p`.

## 4. Output

### 4.1 Audio and video

For each packet:

1. A packet flagged corrupt is dropped, with a warning naming the stream. A
   muxer cannot check a copied packet, and a corrupt one would reach every
   player of the output.
2. The packet goes to the duration helper. When it emits a chunk of length
   `L` with packets `P₁…Pₙ`, the field receives copy content with the copy
   parameters and one data chunk
   `{length = L, stream index, time base = the stream time base, data = [(0, P₁) … (0, Pₙ)]}`,
   each packet tagged audio or video.
3. When the helper emits nothing, the decoder puts an empty run carrying the
   stream index, so every frame of the field holds a run with the source id
   (the empty-run guarantee of [content.md §4](content.md#4-two-chunk-layers)).
   A leading gap before the stream's first packet (§5) is an empty run of the
   same kind.

### 4.2 Subtitles

For each packet, `process` gives:

- `position`: the packet PTS converted from the stream time base to ticks,
  minus the stream origin
  ([decoder.md §8.3](decoder.md#83-positions-and-sparse-advance)); with no
  PTS, the reached position of the timeline;
- `duration`: the packet duration converted the same way, 1 when absent;
- `data`: `[packet]`; a packet flagged corrupt is dropped with a warning and
  gives `[]`.

The timeline's `output` puts copy content with the copy parameters and one
data chunk
`{length, stream index, time base = the stream time base, data = each packet at offset 0, tagged subtitle}`.
A gap between subtitles is a chunk with no packets. Delaying a packet
([decoder.md §16.2](decoder.md#162-subtitle-timeline), step 4) leaves it
unchanged: the packet keeps its own timestamps.

## 5. Timestamps

- Packets keep their PTS, DTS and duration as demuxed, in the stream time
  base.
- Audio and video packets are positioned by DTS: content position =
  `rescale(DTS) − B`, with `B` the mapping origin, for an all-copy mapping
  the smallest first released DTS among its non-sparse streams
  ([decoder.md §8.3](decoder.md#83-positions-and-sparse-advance)). A stream
  whose first DTS is after `B` starts with an empty run of that length.
- Audio and video chunk lengths are steps between the positions of
  consecutive DTS, computed from absolute timestamps with the layer's
  `rescale` ([common.md §5](common.md#5-time-base-conversion),
  [common.md §6.0](common.md#60-absolute-origin)). The content of a stream is
  as long as its DTS span to within one tick, whatever its length; drift
  between streams is owned by [common.md §6](common.md#6-duration-helper).
  Packets with equal DTS, or with no DTS, travel with the group before them.
- Decreasing DTS never reach the decoder
  ([decoder.md §9.2](decoder.md#92-discontinuities-and-invalid-timestamps)).
- Subtitle packets are sparse and positioned by PTS, the cue's
  presentation time: `rescale(PTS) − B`, like the `advance` positions.
- One mapping has one stream index for all its copy streams. A seek and a
  discontinuity give a new one; the encoder then rebases and waits for a
  keyframe ([timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what),
  [encoder-copy.md §12](encoder-copy.md#12-source-switching-and-per-source-state)).

Conformance:

- A 600.023 s Matroska file with one AAC stream (time base 1/1000, 44100 Hz)
  requested as `source(audio=ffmpeg.copy)` gives content 600.023 s long, to
  within one tick.

## 6. Flush

Copy decoders drain on `Flush` ([draining.md](draining.md)):

| Decoder      | `Flush`                                                                                                                  |
| ------------ | ------------------------------------------------------------------------------------------------------------------------ |
| audio, video | flushes the helper and puts its chunk: the held packets, as long as their end per [common.md §6.3](common.md#63-flush-t) |
| subtitles    | the timeline's flush: the pending packets as a chunk of their duration                                                   |

`switch i`: audio and video flush the helper under the old stream index and
continue with a new helper on stream index `i`
([decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments)); subtitles
put later chunks with stream index `i`.

The last packet of every copied stream of a file reaches the field. That it
also reaches the output depends on the end-of-track rule of
[decoder.md §13.1](decoder.md#131-end-of-input).

## 7. Sparse advance

Copy subtitles: the timeline's `advance`
([decoder.md §16.2](decoder.md#162-subtitle-timeline)). Audio and video are
not sparse.

## 8. Errors and logs

Classes are those of [faults.md §2](faults.md#2-classes); escalation and
the bound are [decoder.md §8.2](decoder.md#82-one-decode-step)'s.

| Event                                                    | Class        | Effect                                                                                               |
| -------------------------------------------------------- | ------------ | ---------------------------------------------------------------------------------------------------- |
| unification failure at creation                          | source fault | the track does not start                                                                             |
| corrupt packet                                           | item fault   | warning; audio/video packet dropped, subtitle packet replaced by an empty chunk; counted             |
| decreasing DTS seen by the helper                        | item fault   | packet dropped with a warning; counted                                                               |
| helper "discontinuity" (run past the bound, group bound) | source fault | helper flushed; end of track ([decoder.md §13.2](decoder.md#132-errors))                             |
| subtitle that ended before the reached position          | item fault   | warning; dropped; counted, since it carries media ([faults.md §4](faults.md#4-item-faults-escalate)) |
