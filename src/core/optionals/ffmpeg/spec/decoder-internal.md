# Internal per-stream decoders

This file owns the per-stream decoders that turn decoded FFmpeg frames into
Liquidsoap's internal content: PCM audio, internal video, text subtitles, and
bitmap subtitles rendered as video. It also owns two helpers others reuse:
the internal video converter (§2.2), used by the inline decoders of
[inline-codecs.md](inline-codecs.md), and the subtitle timeline (§7.1), used
by the copy subtitle decoder of [decoder-copy.md](decoder-copy.md). It follows
the contract of [decoder.md, Per-stream decoder contract](decoder.md#7-per-stream-decoder-contract), with
the same section numbers. It relies on the display, frame rate and fit helpers
of [filters.md §16](filters.md#16-helper-graphs) and on the content types of
[content.md](content.md).

## 1. Selection

| Decoder          | Stream           | Requested format                         | Created with                                                                                                                                          |
| ---------------- | ---------------- | ---------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| audio            | audio, frames    | PCM: float (`pcm`), `pcm_s16`, `pcm_f32` | channel count and PCM kind of the format, the field, the codec parameters                                                                             |
| video            | video, frames    | internal video                           | width, height and alpha of the format ([decoder.md §6](decoder.md#6-stream-selection-and-field-mapping)), the stream, the field, the codec parameters |
| text subtitles   | subtitle, frames | text subtitles                           | the field                                                                                                                                             |
| bitmap subtitles | subtitle, frames | internal video                           | the field, width and height of the format                                                                                                             |

## 2. Construction

### 2.1 Audio

1. A converter is chosen by PCM kind: float planar arrays, signed 16-bit
   planar, or 32-bit float planar. Any other kind raises the content's
   `Invalid` exception.
2. The input description starts from the codec parameters: sample rate,
   channel layout, sample format.
3. The output is the audio rate setting and the default channel layout for the
   format's channel count; the sample format is the converter's.
4. A resampler is built from input to output.

### 2.2 Internal video converter

Shared by the internal video decoder and the inline decoders. Built with a
target width `W`, height `H`, and a function giving the target pixel format
for a frame format. It holds at most one **stage**: a stage is built for one
frame format, one time base and one stream index.

`convert ?stream_idx ?format ~time_base ~put frame`:

1. `stream_idx` defaults to 0; `format` defaults to the frame's own format.
2. When the current stage has the same stream index, the same time base and a
   format equal to `format` by the shared video-format comparison
   ([common.md, Video format comparison](common.md#9-video-format-comparison)), it is reused.
3. Otherwise the current stage is flushed (as `flush`), `Video format change.`
   is logged at level 3 when a stage existed, and a new stage is built:
   - fitted size `(fw, fh)`: the largest size inside `W × H` that shows the
     frame format with its proportions, square destination pixels
     ([filters.md §16.4](filters.md#164-fit));
   - a scaler from the frame size and pixel format to `fw × fh` and the
     target pixel format, using the configured scaling threads;
   - a frame rate converter for the format and time base to the video rate
     setting ([filters.md §16.2](filters.md#162-fps)).
4. The frame goes through the frame rate converter; each output frame is
   scaled, wrapped as an image, translated by `((W − fw) / 2, (H − fh) / 2)`
   (integer division), clipped to a `W × H` viewport, and given to `put` with
   the frame it came from.

`flush ~put ()` drains the current stage's frame rate converter through the
same scaling into `put`. It keeps the stage.

### 2.3 Video

1. The target pixel format is YUVA 4:2:0 when the format has alpha, YUV 4:2:0
   otherwise.
2. An internal video converter (§2.2) is built with the format's width and
   height.
3. The stream's pixel aspect is the display pixel aspect of the stream
   (stored on the stream, else in the codec parameters), inverted by a quarter
   turn ([side-data.md §3](side-data.md#3-autorotate-on-decoding)).
4. A display stage is built from the codec parameters and the stream time
   base.

### 2.4 Text subtitles

A subtitle timeline (§7.1) whose items are lists of subtitles.

### 2.5 Bitmap subtitles

1. A video generator of size `W × H` (the format's), with alpha.
2. An empty list of active canvases `(start, stop, canvas)`, a current
   position `pos = 0`, and a cached palette scaler keyed by rectangle size.

## 3. Input

| Decoder                | Items                                                         |
| ---------------------- | ------------------------------------------------------------- |
| audio, video           | decoded frames, then `Flush`                                  |
| text, bitmap subtitles | decoded subtitle frames, then `Flush`; `advance p` calls (§7) |

## 4. Output

### 4.1 Audio

For each frame:

1. When the frame's sample rate, the default layout for its channel count, or
   its sample format differs from the current input description, logs at
   level 3 `Frame format change detected!`, takes the frame's values as the
   new input description and rebuilds the resampler.
2. Converts the whole frame and puts the result on the field.
3. When the frame carries metadata, adds it to the output.

The output goes straight onto the field, not through the generic decoding
buffer's resampler.

### 4.2 Video

For each frame:

1. The display stage turns it into zero or more upright frames
   ([side-data.md §3](side-data.md#3-autorotate-on-decoding)).
2. Each one's frame format gets the stream's pixel aspect when it has none,
   and goes through the converter (§2.2) with the stream time base.
3. Each image is given to the generic decoding buffer's video entry for the
   field, declared at the video rate setting (frames per second / 1). Since the
   rate matches, the buffer keeps one image per frame, scales it to the
   content type's size (already its size) and puts it with a length of one
   video frame (ticks per second / video rate; 1764 with defaults).
4. When the frame carries metadata, adds it to the output.

### 4.3 Text subtitles

For each subtitle frame:

1. `position` is the subtitle PTS (none → 0) converted from `1/1000000` to
   ticks.
2. Each rectangle of type ASS or text becomes a subtitle: `start_time` and
   `end_time` are the display times in milliseconds converted to ticks
   (seconds × main rate, truncated), relative to `position`; `text` is the
   rectangle's ASS text or plain text; `format` is `ass` or `text`; `forced`
   is the rectangle's forced flag. Other rectangles are skipped.
3. `duration` is the largest `end_time`, 0 when none.
4. `(position, duration, subtitles)` goes to the timeline (§7.1). Each output
   chunk holds the subtitles at offset 0.

Example (from the implementation, an SRT stream in Matroska decoded to
`subtitles`, as seen by `on_subtitle`):

```
{forced=false, format="ass", text="0,0,Default,,0,0,0,,First line", absolute_end_time=1.5, absolute_start_time=0.5, end_time=44100, start_time=0, position=22050}
{forced=false, format="ass", text="1,0,Default,,0,0,0,,Second line", absolute_end_time=2.8, absolute_start_time=2.0, end_time=35280, start_time=0, position=88200}
```

### 4.4 Bitmap subtitles

For each subtitle frame:

1. `pts` is the subtitle PTS (none → 0) converted from `1/1000000` to ticks;
   `start` and `stop` are the display times converted to ticks as in §4.3.
2. A `W × H` transparent canvas receives each bitmap rectangle: its palette
   picture (`w × h`) is converted to YUVA 4:2:0 at the same size and placed at
   `(x, y)`, unscaled. Other rectangles are skipped.
3. `(pts + start, pts + stop, canvas)` joins the active list.

Nothing is put until `advance` (§7.2).

## 5. Timestamps

| Decoder          | Reads                                        | Effect                                                                                                                    |
| ---------------- | -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| audio            | none                                         | length = number of output samples                                                                                         |
| video            | frame PTS, stream time base                  | drives the frame rate converter, which drops or repeats frames to the video rate ([filters.md §16.2](filters.md#162-fps)) |
| text subtitles   | subtitle PTS, `1/1000000`; display times, ms | absolute position of each subtitle on the field timeline                                                                  |
| bitmap subtitles | same                                         | absolute display interval                                                                                                 |

Audio gaps and timestamp jumps do not change the output: samples are put
back to back. Subtitle positions are absolute timestamps, while the
`advance` positions of [decoder.md §8.3](decoder.md#83-positions-and-sparse-advance) are relative to each
stream's first timestamp.

## 6. Flush

| Decoder          | `Flush`                                                                      |
| ---------------- | ---------------------------------------------------------------------------- |
| audio            | nothing; samples held by the resampler are not drained                       |
| video            | drains the display stage through the converter, then flushes the converter   |
| text subtitles   | the timeline's flush (§7.1)                                                  |
| bitmap subtitles | `advance` to the largest `stop` among active canvases, or to `pos` when none |

## 7. Sparse advance

### 7.1 Subtitle timeline

Shared by the text subtitle decoder and the copy subtitle decoder. Built with
`process` (item → `(position, duration, data)`) and `output ?data ~length`
(puts one chunk of `length` ticks on the field, holding `data` at offset 0, or
nothing). State: none, or `(p, pending)` with `p` the position reached and
`pending` an optional `(duration, data)`.

On an item, with `(q, d, data) = process item`:

1. State `(p, _)` with `q ≤ p`: the item is dropped.
2. State `(p, pending)`: outputs `pending`'s data (or nothing) with length
   `q − p`; state becomes `(q, (d, data))`.
3. No state: when `q > 0`, outputs nothing with length `q`; state becomes
   `(q, (d, data))`.

On `advance r`:

1. No state: outputs nothing with length `r`; state becomes `(r, none)`.
2. State `(p, _)` with `r ≤ p`: nothing.
3. State `(p, pending)`: outputs `pending`'s data (or nothing) with length `r
− p`; state becomes `(r, none)`.

On flush: when a pending `(d, data)` exists, outputs `data` with length `d`.

A subtitle is thus held at its position until the next item or the next
advance, and its chunk spans the gap to that point.

### 7.2 Bitmap subtitles

`advance r`:

1. Generates `r − pos` ticks of video. For each video frame at position `t`
   (`pos` plus its offset in the generated span), the frame is a transparent
   `W × H` canvas with every active canvas whose `start ≤ t < stop` added; an
   active canvas with `stop ≤ t` is removed from the list.
2. Puts the generated video on the field; `pos = r`.

### 7.3 Audio and video

Not sparse.

## 8. Errors and logs

| Decoder   | Event                                        | Effect                                                                               |
| --------- | -------------------------------------------- | ------------------------------------------------------------------------------------ |
| audio     | unsupported PCM kind at creation             | raises the content's `Invalid`; decoder creation fails                               |
| audio     | format change                                | level 3 `Frame format change detected!`, resampler rebuilt                           |
| video     | format, time base or stream change           | level 3 `Video format change.`, stage rebuilt                                        |
| subtitles | item at or before the reached position       | dropped silently                                                                     |
| subtitles | unsupported rectangle type                   | skipped silently                                                                     |
| all       | errors from the resampler, scaler or filters | propagate: the track ends ([decoder.md §13](decoder.md#13-end-of-stream-and-errors)) |
