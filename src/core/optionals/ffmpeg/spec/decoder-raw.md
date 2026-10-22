# Raw per-stream decoders

This file owns the per-stream decoders that keep decoded FFmpeg frames as
they are, in the raw content types `ffmpeg.audio.raw` and `ffmpeg.video.raw`
of [content.md](content.md). It follows the contract of
[decoder.md, Per-stream decoder contract](decoder.md#7-per-stream-decoder-contract), with the same section
numbers. It relies on the audio format and display helpers of
[filters.md §16](filters.md#16-helper-graphs), on the raw video conformer of
[content.md](content.md), and on the grouping helper of
[decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments).

## 1. Selection

| Decoder   | Stream        | Requested format   | Created with                                                              |
| --------- | ------------- | ------------------ | ------------------------------------------------------------------------- |
| raw audio | audio, frames | `ffmpeg.audio.raw` | the stream index, the format, the stream, the field, the codec parameters |
| raw video | video, frames | `ffmpeg.video.raw` | the same                                                                  |

## 2. Construction

### 2.1 Raw audio

1. The destination parameters are the requested format's channel layout,
   sample format and sample rate, each possibly undecided. At probe time they
   were unified with the stream's ([stream-description.md, `get_type`](stream-description.md#6-get_type-ctype-container)).
2. An audio format stage converts from the codec parameters' layout, sample
   format and rate, with the stream time base as input time base, to the
   destination parameters; an undecided parameter keeps the source's
   ([filters.md, AFormat](filters.md#163-aformat)).
3. A grouping helper (§5) in PTS mode on the stage's output time base.

### 2.2 Raw video

1. A display stage from the codec parameters and the stream time base.
2. A conformer for the requested format ([content.md §9](content.md#9-raw-video-conformance)): the first frame decides the undecided parameters of the
   format; a later frame in another format is fitted to it.
3. A grouping helper in PTS mode on the stream time base.

## 3. Input

Decoded frames, then `Flush`.

## 4. Output

### 4.1 Raw audio

Each frame goes through the audio format stage; each output frame goes to the
grouping helper. Each chunk the helper emits is put on the field as raw audio
content with the destination parameters, one data chunk `{length, stream
index, time base = the stage's output time base, frames at offset 0}`.

### 4.2 Raw video

Each frame goes through the display stage, then the conformer; each output
frame goes to the grouping helper. Each emitted chunk is put on the field as
raw video content whose parameters are those of the format **at the time of
the put** (so the first frame's parameters once the conformer has decided
them), with `{length, stream index, time base = the stream time base, frames at
offset 0}`.

## 5. Timestamps

- Frames keep their PTS in their time base: the helper is used with no time
  base conversion and offset 0.
- A chunk's length is the PTS step to the next frame, in ticks, truncated
  ([decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments)). Frames with equal PTS, or a step under one
  tick, are grouped in one chunk.
- A frame with no PTS joins the held group.
- A PTS lower than the previous one raises (§8).

A seek gives a new stream index ([decoder.md §12](decoder.md#12-seeking)); what the
encoder does with it is in [timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what).

## 6. Flush

| Decoder   | `Flush`                                                                                          |
| --------- | ------------------------------------------------------------------------------------------------ |
| raw audio | drains the audio format stage into the helper; the helper itself is not flushed                  |
| raw video | drains the display stage through the conformer into the helper; the helper itself is not flushed |

The held group (the last frame, at least) is not emitted.

## 7. Sparse advance

Not sparse.

## 8. Errors and logs

| Event                                               | Effect                                                                                                                                                                           |
| --------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| non-monotonic PTS                                   | level 3 `Invalid ffmpeg content: non-monotonic PTS: <ts> < <last>`, the content's `Invalid` is raised; the track ends ([decoder.md §13](decoder.md#13-end-of-stream-and-errors)) |
| unification failure in the conformer                | raises; the track ends                                                                                                                                                           |
| errors from the audio format, display or fit stages | propagate; the track ends                                                                                                                                                        |
