# Internal per-stream decoders

This file owns the per-stream decoders that turn decoded FFmpeg frames into
Liquidsoap's internal content: PCM audio, internal video, text subtitles, and
bitmap subtitles rendered as video. It answers every clause of
[decoder.md, Per-stream decoder contract](decoder.md#7-per-stream-decoder-contract)
with the same section numbers. It uses the internal video converter and the
subtitle timeline of [decoder.md §16](decoder.md#16-shared-helpers), the
display stage of [side-data.md §3](side-data.md#3-autorotate-on-decoding),
and the content types of [content.md](content.md).

## 1. Selection

| Decoder          | Stream           | Requested format                         | Created with                                                                                                                                          |
| ---------------- | ---------------- | ---------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| audio            | audio, frames    | PCM: float (`pcm`), `pcm_s16`, `pcm_f32` | channel count and PCM kind of the format, the field, the codec parameters                                                                             |
| video            | video, frames    | internal video                           | width, height and alpha of the format ([decoder.md §6](decoder.md#6-stream-selection-and-field-mapping)), the stream, the field, the codec parameters |
| text subtitles   | subtitle, frames | text subtitles                           | the field                                                                                                                                             |
| bitmap subtitles | subtitle, frames | internal video                           | the field, width and height of the format, the codec parameters                                                                                       |

## 2. Construction

### 2.1 Audio

1. A converter is chosen by PCM kind: float planar arrays, signed 16-bit
   planar, or 32-bit float planar. Any other kind is an error and the decoder
   creation fails.
2. The input description is the codec parameters' sample rate, channel
   layout and sample format. A parameter set with no channel layout takes the
   default layout for its channel count.
3. The output is the audio rate setting and the default channel layout for the
   format's channel count; the sample format is the converter's.
4. A resampler is built from input to output.

### 2.2 Video

1. The target pixel format is YUVA 4:2:0 when the format has alpha, YUV 4:2:0
   otherwise.
2. An internal video converter
   ([decoder.md §16.1](decoder.md#161-internal-video-converter)) is built with
   the format's width and height.
3. The stream's pixel aspect is the display pixel aspect of the stream
   (stored on the stream, else in the codec parameters), inverted by a quarter
   turn ([side-data.md §3](side-data.md#3-autorotate-on-decoding)).
4. A display stage is built from the codec parameters and the stream time
   base.

### 2.3 Text subtitles

A subtitle timeline ([decoder.md §16.2](decoder.md#162-subtitle-timeline))
whose data are lists of subtitles; merging concatenates the lists.

### 2.4 Bitmap subtitles

1. A video generator of size `W × H` (the format's), with alpha.
2. The subtitle canvas `cw × ch`: the width and height of the codec
   parameters, or `W × H` when they are not set. Rectangle coordinates are in
   this canvas.
3. A placement that fits `cw × ch` into `W × H`, proportions kept, centred
   ([filters.md §16.4](filters.md#164-fit)).
4. An empty list of active canvases `(start, stop, canvas)` and a current
   position `pos = 0`.

## 3. Input

| Decoder                | Items                                                        |
| ---------------------- | ------------------------------------------------------------ |
| audio, video           | decoded frames; `switch`; `Flush`                            |
| text, bitmap subtitles | decoded subtitle frames; `switch`; `advance p` (§7); `Flush` |

## 4. Output

### 4.1 Audio

For each frame:

1. The frame's description is its sample rate, its channel layout (the
   default layout for its channel count when the frame has none) and its
   sample format. When it differs from the current input description, the
   decoder drains the resampler onto the field, logs a format change at
   important level, takes the frame's description and rebuilds the
   resampler.
2. Converts the whole frame and puts the result on the field.
3. When the frame carries metadata, adds it to the output.

The output goes straight onto the field, not through the generic decoding
buffer's resampler.

### 4.2 Video

For each frame:

1. The display stage turns it into zero or more upright frames
   ([side-data.md §3](side-data.md#3-autorotate-on-decoding)).
2. Each one's frame format gets the stream's pixel aspect when it has none,
   and goes through the converter with the stream time base and the current
   stream index.
3. Each image is given to the generic decoding buffer's video entry for the
   field, declared at the video rate setting. Since the rate matches, the
   buffer keeps one image per frame and puts it with a length of one video
   frame (ticks per second / video rate; 1764 with defaults).
4. When the frame carries metadata, adds it to the output.

### 4.3 Text subtitles

For each subtitle frame:

1. `position` is the subtitle PTS converted from `1/1000000` to ticks, minus
   the stream origin ([decoder.md §8.3](decoder.md#83-positions-and-sparse-advance)).
   A subtitle with no PTS takes the reached position of the timeline.
2. Each rectangle of type ASS or text becomes a subtitle: `start_time` and
   `end_time` are the display times in milliseconds converted to ticks,
   relative to `position`; `text` is the rectangle's ASS text or plain text;
   `format` is `ass` or `text`; `forced` is the rectangle's forced flag.
3. A rectangle of another type is skipped with a warning: a bitmap subtitle
   is decoded to video, not to text.
4. `duration` is the largest `end_time`, 0 when none.
5. `(position, duration, subtitles)` goes to the timeline. Each output chunk
   holds the subtitles at offset 0.

Delaying a subtitle by `d` ticks
([decoder.md §16.2](decoder.md#162-subtitle-timeline), step 4) lowers its
`start_time` and `end_time` by `d`, at least to 0.

Example: an SRT stream in Matroska decoded to `subtitles`, as seen by
`on_subtitle`:

```
{forced=false, format="ass", text="0,0,Default,,0,0,0,,First line", absolute_end_time=1.5, absolute_start_time=0.5, end_time=44100, start_time=0, position=22050}
{forced=false, format="ass", text="1,0,Default,,0,0,0,,Second line", absolute_end_time=2.8, absolute_start_time=2.0, end_time=35280, start_time=0, position=88200}
```

### 4.4 Bitmap subtitles

For each subtitle frame:

1. `pts` is the subtitle PTS converted from `1/1000000` to ticks, minus the
   stream origin; with no PTS, `pos`. `start` and `stop` are the display
   times converted to ticks as in §4.3.
2. A `W × H` transparent canvas receives each bitmap rectangle: its palette
   picture is converted to YUVA 4:2:0 and placed through the canvas
   placement of §2.4: position and size scaled from `cw × ch` to the fitted
   area, offset by its centring. A rectangle of another type is skipped with
   a warning.
3. `(pts + start, pts + stop, canvas)` joins the active list.

Nothing is put until `advance` (§7.2).

Conformance:

- A DVD subtitle stream with a 720×576 canvas over a 1280×720 video: each
  rectangle lands at the place it occupies on the DVD picture, scaled.

## 5. Timestamps

| Decoder          | Reads                                                                                             | Effect                                                                                                                    |
| ---------------- | ------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| audio            | none                                                                                              | length = number of output samples                                                                                         |
| video            | frame timestamp ([decoder.md §9](decoder.md#9-timestamps-on-the-decoding-side)), stream time base | drives the frame rate converter, which drops or repeats frames to the video rate ([filters.md §16.2](filters.md#162-fps)) |
| text subtitles   | subtitle PTS, `1/1000000`; display times, ms                                                      | position of each subtitle on the field, from the stream origin                                                            |
| bitmap subtitles | same                                                                                              | display interval on the field, from the stream origin                                                                     |

Decoded audio is continuous by construction: samples are put back to back,
and timestamps do not change the output. Subtitle positions and `advance`
positions count from the same origin
([decoder.md §8.3](decoder.md#83-positions-and-sparse-advance)), so subtitles
stay in place on a file whose start time is not zero.

## 6. Flush

Every internal decoder drains on `Flush` ([draining.md](draining.md)).

| Decoder          | `Flush`                                                                      |
| ---------------- | ---------------------------------------------------------------------------- |
| audio            | drains the resampler and puts what it returns on the field                   |
| video            | drains the display stage through the converter, then flushes the converter   |
| text subtitles   | the timeline's flush                                                         |
| bitmap subtitles | `advance` to the largest `stop` among active canvases, or to `pos` when none |

| Decoder      | `switch i`                                                                             |
| ------------ | -------------------------------------------------------------------------------------- |
| audio        | nothing: samples are continuous and carry no stream index                              |
| video        | later frames use stream index `i`; the converter drains its stage and builds a new one |
| text, bitmap | nothing: positions continue (the loop moves the stream origin)                         |

Conformance:

- An AAC file decoded to PCM gives the same number of samples, resampled,
  as the file holds.

## 7. Sparse advance

### 7.1 Text subtitles

The timeline's `advance` ([decoder.md §16.2](decoder.md#162-subtitle-timeline)).

### 7.2 Bitmap subtitles

`advance r`:

1. Generates `r − pos` ticks of video. For each video frame at position `t`
   (`pos` plus its offset in the generated span), the frame is a transparent
   `W × H` canvas with every active canvas whose `start ≤ t < stop` added; an
   active canvas with `stop ≤ t` is removed from the list. A canvas whose
   `start` is before `pos` shows from `pos` on.
2. Puts the generated video on the field; `pos = r`.

### 7.3 Audio and video

Not sparse.

## 8. Errors and logs

Classes are those of [faults.md §2](faults.md#2-classes); escalation and
the bound are [decoder.md §8.2](decoder.md#82-one-decode-step)'s.

| Decoder   | Event                                                            | Class        | Effect                                                                                                    |
| --------- | ---------------------------------------------------------------- | ------------ | --------------------------------------------------------------------------------------------------------- |
| audio     | unsupported PCM kind at creation                                 | source fault | the track does not start                                                                                  |
| audio     | format change                                                    | —            | important log, resampler drained and rebuilt                                                              |
| video     | format, time base or stream index change                         | —            | important log, stage drained and rebuilt                                                                  |
| subtitles | unsupported rectangle type, the cue's other rectangles delivered | item fault   | warning; rectangle skipped; not counted: the cue still delivers media                                     |
| subtitles | a cue none of whose rectangles is supported                      | item fault   | warning; cue dropped; counted                                                                             |
| subtitles | item ended before the reached position                           | item fault   | warning; item dropped; counted, since it carries media ([faults.md §4](faults.md#4-item-faults-escalate)) |
| all       | the resampler, scaler or a filter fails on an item               | item fault   | item dropped with a warning; counted                                                                      |
| all       | a stage cannot be rebuilt after a format change                  | source fault | end of track ([decoder.md §13.2](decoder.md#132-errors))                                                  |
