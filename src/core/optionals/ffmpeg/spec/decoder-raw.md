# Raw per-stream decoders

This file owns the per-stream decoders that keep decoded FFmpeg frames as
they are, in the raw content types `ffmpeg.audio.raw` and `ffmpeg.video.raw`
of [content.md](content.md). It answers every clause of
[decoder.md, Per-stream decoder contract](decoder.md#7-per-stream-decoder-contract)
with the same section numbers. It relies on the audio format and display
stages ([filters.md §16](filters.md#16-helper-graphs),
[side-data.md §3](side-data.md#3-autorotate-on-decoding)), on the raw video
conformer of [content.md §9](content.md#9-raw-video-conformance), and on the
duration helper as [decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments)
configures it.

## 1. Selection

| Decoder   | Stream        | Requested format   | Created with                                                              |
| --------- | ------------- | ------------------ | ------------------------------------------------------------------------- |
| raw audio | audio, frames | `ffmpeg.audio.raw` | the stream index, the format, the stream, the field, the codec parameters |
| raw video | video, frames | `ffmpeg.video.raw` | the same                                                                  |

## 2. Construction

### 2.1 Raw audio

1. The destination parameters are the requested format's channel layout,
   sample format and sample rate, each possibly undecided. At probe time they
   were unified with the stream's
   ([stream-description.md §6](stream-description.md#6-get_type-ctype-container)).
2. An audio format stage converts from the codec parameters' layout, sample
   format and rate, with the stream time base as input time base, to the
   destination parameters; an undecided parameter keeps the source's
   ([filters.md §16.3](filters.md#163-aformat)).
3. A duration helper in PTS mode on the stage's output time base.

### 2.2 Raw video

1. A display stage from the codec parameters and the stream time base.
2. A conformer for the requested format
   ([content.md §9](content.md#9-raw-video-conformance)): the first frame
   decides the undecided parameters of the format; a later frame in another
   format is fitted to it, without a track mark.
3. A duration helper in PTS mode on the stream time base.

## 3. Input

Decoded frames; `switch`; `Flush`.

## 4. Output

### 4.1 Raw audio

Each frame goes through the audio format stage; each output frame goes to the
duration helper. Each chunk the helper emits is put on the field as raw audio
content with the destination parameters, one data chunk
`{length, stream index, time base = the stage's output time base, frames at offset 0}`.

### 4.2 Raw video

Each frame goes through the display stage, then the conformer; each output
frame goes to the duration helper. Each emitted chunk is put on the field as
raw video content whose parameters are those of the format at the time of
the put (so the first frame's parameters once the conformer has decided
them), with
`{length, stream index, time base = the stream time base, frames at offset 0}`.

## 5. Timestamps

- The frame timestamp is that of
  [decoder.md §9](decoder.md#9-timestamps-on-the-decoding-side) (PTS, else
  best-effort); frames keep it in their time base.
- A chunk's length is the step between the positions of consecutive frames,
  computed from absolute timestamps
  ([decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments)). Frames
  at equal positions, and frames with no PTS, travel in one group.
- Decreasing PTS never reach the decoder
  ([decoder.md §9.2](decoder.md#92-discontinuities-and-invalid-timestamps)).

The stream index of the content changes at a seek and at a discontinuity;
what the encoder does with a new one is in
[timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what).

## 6. Flush

Raw decoders drain on `Flush` ([draining.md](draining.md)), upstream first:

| Decoder   | `Flush`                                                                                                    |
| --------- | ---------------------------------------------------------------------------------------------------------- |
| raw audio | drains the audio format stage into the helper, then flushes the helper and puts its chunk                  |
| raw video | drains the display stage through the conformer into the helper, then flushes the helper and puts its chunk |

`switch i` drains like `Flush`, upstream first, under the old stream index:
the audio format or display stage, then the helper. The decoder then
continues with fresh stages and a new helper on stream index `i`
([decoder.md §9.1](decoder.md#91-grouping-by-timestamp-increments)), so no
old-timeline frame enters the new helper ([decoder.md §7.6](decoder.md#76-flush)).

Conformance:

- A 2 s AAC file decoded as `ffmpeg.audio.raw` and written to wav gives the
  same number of samples as the internal path (88200 at 44.1 kHz).

## 7. Sparse advance

Not sparse.

## 8. Errors and logs

Classes are those of [faults.md §2](faults.md#2-classes); escalation and
the bound are [decoder.md §8.2](decoder.md#82-one-decode-step)'s.

| Event                                                    | Class        | Effect                                                                   |
| -------------------------------------------------------- | ------------ | ------------------------------------------------------------------------ |
| unification failure in the conformer                     | item fault   | frame dropped with a warning; counted                                    |
| the audio format, display or fit stage fails on a frame  | item fault   | frame dropped with a warning; counted                                    |
| decreasing PTS seen by the helper                        | item fault   | frame dropped with a warning; counted                                    |
| helper "discontinuity" (run past the bound, group bound) | source fault | helper flushed; end of track ([decoder.md §13.2](decoder.md#132-errors)) |
