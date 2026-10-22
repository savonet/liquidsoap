# Copy per-stream decoders

This file owns the per-stream decoders that pass demuxed FFmpeg packets
through undecoded, in the `ffmpeg.copy` content type of
[content.md](content.md). It follows the contract of
[decoder.md, Per-stream decoder contract](decoder.md#7-per-stream-decoder-contract), with the same section
numbers. It relies on the grouping helper of [decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments) and
the subtitle timeline of [decoder-internal.md §7.1](decoder-internal.md#71-subtitle-timeline). How
the encoder turns these packets' timestamps into output timestamps is in
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

1. The copy parameters are built:
   - audio: the codec parameters;
   - video: the codec parameters and the stream's average frame rate;
   - subtitles: the codec parameters and the stream time base.
2. The requested format is unified with them ([content.md §3.1](content.md#31-ffmpegcopy)). A failure raises and the decoder creation fails.
3. Audio and video: a grouping helper in DTS mode on the stream time base,
   without time base conversion, offset 0 ([decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments)).
4. Subtitles: a subtitle timeline ([decoder-internal.md](decoder-internal.md)
   §7.1) on the stream time base.

## 3. Input

Packets, then `Flush`. Copy subtitles also receive `advance p` calls.

## 4. Output

### 4.1 Audio and video

For each packet:

1. A packet flagged corrupt logs at level 3 `Corrupted packet in stream!` and
   is dropped.
2. The packet goes to the grouping helper. When it emits a chunk of length
   `L` with packets `P₁…Pₙ`, the field receives copy content with the copy
   parameters and one data chunk `{length = L, stream index, time base = the
stream time base, data = [(0, P₁) … (0, Pₙ)]}`, each packet tagged audio or
   video.
3. When the helper emits nothing, nothing is put.

### 4.2 Subtitles

For each packet, `process` gives:

- `position`: the packet PTS (none → 0) converted from the stream time base to
  ticks;
- `duration`: the packet duration (none → 1) converted the same way;
- `data`: `[packet]`, or `[]` when the packet is flagged corrupt (logged at
  level 3 `Corrupted packet in stream!`).

The timeline's `output ?data ~length` puts copy content with the copy
parameters and one data chunk `{length, stream index, time base = the stream
time base, data = each packet at offset 0, tagged subtitle}`. A gap between
subtitles is a chunk with no packets.

## 5. Timestamps

- Packets keep their PTS and DTS as demuxed, in the stream time base (the
  helper rewrites the DTS with itself plus an offset of 0).
- Audio and video chunk lengths are DTS steps converted to ticks one step at a
  time, each truncated toward zero ([decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments)). A chunk
  holds the packets up to the next increasing DTS: packets with equal DTS, or
  with no DTS, travel with the previous one.
- A DTS lower than the previous one raises (§8).
- Subtitle positions use absolute PTS; gaps become empty chunks; a subtitle
  at or before the reached position is dropped
  ([decoder-internal.md §7.1](decoder-internal.md#71-subtitle-timeline)).
- Each decoder record has one stream index for all its copy streams; a seek
  builds new decoders with a new stream index and a fresh helper, so the
  first packet after a seek starts a new grouping. What the encoder does on a
  new stream index is in [timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what).

Example of the length rule (from the implementation): a 600.023 s Matroska
file with one AAC stream (time base 1/1000, 44100 Hz) requested as
`source(audio=ffmpeg.copy)` plays 599.834 s: each 23 or 24 ms step loses its
fraction of a tick.

## 6. Flush

| Decoder      | `Flush`                                                                                                                                            |
| ------------ | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| audio, video | the helper's flush: the held packets as one chunk whose length is the largest packet duration in ticks, at least 1; nothing when no packet is held |
| subtitles    | the timeline's flush: the pending packet as a chunk of its duration                                                                                |

## 7. Sparse advance

Copy subtitles: the timeline's `advance` ([decoder-internal.md](decoder-internal.md)
§7.1). Audio and video are not sparse.

## 8. Errors and logs

| Event                            | Effect                                                                                                                                                                           |
| -------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| unification failure at creation  | raises; decoder creation fails                                                                                                                                                   |
| corrupt packet                   | level 3 `Corrupted packet in stream!`; audio/video packet dropped, subtitle packet replaced by an empty chunk                                                                    |
| non-monotonic DTS (audio, video) | level 3 `Invalid ffmpeg content: non-monotonic DTS: <ts> < <last>`, the content's `Invalid` is raised; the track ends ([decoder.md §13](decoder.md#13-end-of-stream-and-errors)) |
