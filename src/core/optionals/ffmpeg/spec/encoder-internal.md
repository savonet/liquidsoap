# Internal and raw stream encoders

This file owns the stream encoders that encode content with an FFmpeg codec:
audio and video in `Internal` mode (from Liquidsoap PCM and YUV frames) and
`Raw` mode (from decoded FFmpeg frames carried as `ffmpeg.audio.raw` /
`ffmpeg.video.raw`), and subtitles. It answers every clause of the contract of
[encoder.md §12](encoder.md#12-stream-encoder-contract) with the same
numbering: §1 is §12.1, §2 is §12.2, and so on. It relies on the raw content
layout of [content.md](content.md), on the bindings' stream creation and
`write_frame` ([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)),
on the pixel format, scaling and hardware rules of [common.md](common.md), on
the helper graphs of [filters.md §16](filters.md#16-helper-graphs), and on the
draining rule of [draining.md](draining.md). Timestamps are in
[timestamps.md §3](timestamps.md#3-audio-internal-and-raw)–§5.

## 1. Interface

| Media    | `mk_stream`               | `flush`                                       | `request_keyframe`       | `video_size`                  |
| -------- | ------------------------- | --------------------------------------------- | ------------------------ | ----------------------------- |
| audio    | creates the stream (§2.1) | drains the resampler, then the re-framer (§8) | nothing                  | none                          |
| video    | creates the stream (§2.2) | drains the frame-rate converter (§8)          | key frame at a time (§7) | the stream's width and height |
| subtitle | creates the stream (§2.3) | nothing                                       | nothing                  | none                          |

`codec_attr` and `bitrate` come from the stream (§9).

## 2. Stream creation

At build, for every media: the encoder is looked up by `codec`, and the
option names are checked ([encoder.md §4](encoder.md#4-option-handling)). An
encoder FFmpeg does not have is an error of kind `encoder` naming the codec
(example message:
`Cannot find %ffmpeg encoder <c>. Make sure that ffmpeg is compiled with it!`). The stream is created at `mk_stream`, with the
container's first frame, in track order
([encoder.md §5](encoder.md#5-encoding-a-frame)); every leftover option then
raises the error of [encoder.md §4](encoder.md#4-option-handling).

### 2.1 Audio

Target parameters:

- sample rate = the track's `samplerate`;
- channel layout = the track's named `channel_layout` when there is one, else
  FFmpeg's default layout for `channels`
  ([encoder-format.md §5](encoder-format.md#5-audio-tracks)); a count with no
  default layout is an error of kind `encoder`;
- sample format = FFmpeg's best sample format of the codec for the requested
  one (`sample_format`, default `dbl`).

The stream is added with that sample rate, layout and format, time base
`1/samplerate`, the codec and a copy of the track options. A failure is an
error of kind `encoder` that prints the requested parameters and FFmpeg's
error.

`frame_size` = none when the codec has the variable-frame-size capability,
else the codec's frame size.

### 2.2 Video

At build: the target frame rate, width and height from the track; the
scaling algorithm ([common.md §10](common.md#10-scaling)).

At `mk_stream frame`:

1. `alpha` = the track's `alpha` when set, else whether the frame's content
   type for the field is a video format with alpha.
2. Target pixel format: the encoder pixel format of
   [common.md §7](common.md#7-pixel-formats), from the track's `pixel_format`
   and `alpha`.
3. Hardware context and stream pixel format:
   [common.md §11](common.md#11-hardware-contexts). The **frame pixel
   format**, the one every frame given to the codec carries, is the stream
   pixel format when it is a software format (no `Hwaccel` flag), else the
   target pixel format (a frame context uploads from it).
4. Side data: [side-data.md §7](side-data.md#7-side-data-given-to-encoders).
5. The stream is added: time base `1/framerate`, frame rate `framerate/1`,
   the chosen pixel format and hardware context, width and height, a copy of
   the options.

### 2.3 Subtitle

Stream time base `1/1000000`, default ASS header
([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)).

### 2.4 Parameters across sources

An encoded stream converts every input to its own parameters (§4), so no
source is incompatible with it.

## 3. Input

| Media / mode   | Reads                                                                                           |
| -------------- | ----------------------------------------------------------------------------------------------- |
| audio internal | the field's PCM samples for the frame's audio length (the PCM kind of the track)                |
| audio raw      | every frame of every run of the field, in order of absolute position                            |
| video internal | every image of the field's video data, in order                                                 |
| video raw      | every frame of every run, in order of absolute position, with the run's time base and source id |
| subtitle       | every subtitle of the field, in order                                                           |

A frame of length 0 carries no data and changes nothing. Raw frames are read
from runs at their absolute positions (run start plus item position,
[content.md §4.2](content.md#42-run-layer)); a gap between runs carries no
frame. Each raw frame's parameters are read from the frame itself.

## 4. Conversion

### 4.1 Audio internal

One resampler per stream: from the global audio rate, the frame's channel
layout and the PCM kind's planar format, to the target rate, layout and
sample format. Each `encode` converts the frame's samples and yields one
FFmpeg audio frame. The resampler holds its filter delay.

### 4.2 Audio raw

A resampler is keyed on the input's (sample rate, channel layout, sample
format), read from each frame: identity when all three equal the target, else
an FFmpeg-frame to FFmpeg-frame resampler. A frame whose key differs from the
current resampler's drains the current one into the next stage
([draining.md §2](draining.md#2-the-rule)), then builds a resampler for the
new key. Producers deliver raw audio conformed to the field's format
([content.md §14](content.md#14-raw-audio-conformance)), so within one
content format the key stays the same.

The encoder never invents samples. The sample counter follows the output
position ([timestamps.md §3](timestamps.md#3-audio-internal-and-raw)):

- **Gap.** At a gap in the raw content's positions, the resampler is
  drained, and the sample counter jumps to the next frame's position at the
  target rate. The output carries a timestamp gap. A gap is not a fault: no
  source was present there. With re-framing (§4.3), the re-framer is not
  drained at a gap: FFmpeg accepts a short frame only as a stream's last.
  Its partial frame is completed by the samples after the gap, and the jump
  is applied to that frame's start instead. The gap's timing then moves
  earlier by less than one codec frame; no sample is invented or lost.
- **Overlap.** Samples before the counter's position are dropped with a
  warning naming the overlap: an item fault
  ([faults.md §2](faults.md#2-classes)), counted with the others of
  [encoder.md §5.2](encoder.md#52-data-faults-and-output-faults).

Both are computed from absolute positions, never accumulated.

### 4.3 Re-framing

When `frame_size` is set (§2.1), converted frames go through an audio
re-framer that delivers frames of exactly `frame_size` samples, except the
last one after a drain
([avfilter.md](../../../../modules/synced/ffmpeg/spec/avfilter.md), audio
converter). The re-framer holds up to one partial frame.

### 4.4 Video internal

One scaler, built on the first image: from the global video size and
`yuva420p`/`yuv420p` (alpha when the target pixel format has alpha) to the
target size and the frame pixel format (§2.2), with the scaling threads of
[common.md §10](common.md#10-scaling). Each image is first cut to the global
video size and rendered onto an opaque canvas unless `alpha`, then packed and
scaled. The scaler holds nothing.

### 4.5 Video raw

A frame of the target width, height and frame pixel format (§2.2) passes as
is. Any other is fitted: scaled into the target size keeping its
proportions, square target pixels, padded, converted to the frame pixel
format, timestamps kept ([filters.md §16.4](filters.md#164-fit)). Reason: the
codec encodes one pixel format, fixed when the stream is created; a frame in
another one would be rejected or misread. Frames of different sizes,
pixel formats or colour properties mid-stream are each fitted; none ends the
track.

### 4.6 Frame-rate conversion (both video modes)

Every video frame then goes through a frame-rate converter, the Fps helper
graph of [filters.md §16.2](filters.md#162-fps), with input time base main
ticks. Frames enter at their output position
([timestamps.md §4.1](timestamps.md#41-frame-timestamps-entering-the-converter));
the converter applies no `setpts` re-anchoring. It continues the output grid
through its start time: a converter built when `W` frames are written starts
at grid slot `W`
([timestamps.md §4.3](timestamps.md#43-building-and-re-anchoring-a-converter)).

The converter is keyed on the frame format alone (the set of properties that
count as a format change is [common.md §9](common.md#9-video-format-comparison)'s);
a source switch does not rebuild it. It is drained and rebuilt:

- when a frame's format differs from the current converter's;
- at a gap in the raw content's positions: the converter is drained before
  the gap and a new one starts after it, so it never repeats a frame across
  a gap ([faults.md §3](faults.md#3-source-faults-reach-the-script): no
  invented content).

Draining tells the converter end of stream; every frame it delivers is
encoded and written before the new converter receives a frame.

### 4.7 Subtitle

Each subtitle becomes one FFmpeg subtitle with one ASS rectangle:

- ASS text = the subtitle's text when its format is ASS, else
  `text_to_ass(n, text)` where `n` counts the subtitles of this stream from 0;
- flags = `forced` when the subtitle is forced;
- display times: [timestamps.md §5](timestamps.md#5-subtitles).

### 4.8 Conformance

- Three raw-audio files (48 kHz stereo, 44.1 kHz mono, 44.1 kHz stereo) in
  sequence into one AAC stream at 44100 Hz: the output's duration equals the
  inputs' total within one codec frame, and each part plays at its pitch.
- A raw video stream whose source changes every second for ten seconds: the
  output has `10 × framerate` frames, within one frame.

## 5. Timestamps

### 5.1 Audio

[timestamps.md §3](timestamps.md#3-audio-internal-and-raw). The audio frame
writer (timestamping, optional re-framing, then `write_frame`) is shared with
the inline encoders.

### 5.2 Video

[timestamps.md §4](timestamps.md#4-video-internal-and-raw).

### 5.3 Subtitle

[timestamps.md §5](timestamps.md#5-subtitles).

## 6. Writing

Audio and video: the bindings' `write_frame`, which encodes and writes every
packet produced, converting from the encoder's time base to the stream's.
Subtitles: `write_subtitle_frame`. A frame or subtitle the codec rejects, and
a packet the muxer rejects, is an item fault: dropped with a warning, and the
stream goes on, up to the bound of
[encoder.md §5.2](encoder.md#52-data-faults-and-output-faults). An I/O error
of the byte sink is an output fault ([faults.md §2](faults.md#2-classes)).

## 7. Keyframes

Every packet goes through the instance's hook with its key flag before it
reaches the muxer ([encoder.md §8.2](encoder.md#82-keyframe-hooks)). Subtitle
packets count as key packets.

`request_keyframe t` on a video stream marks the first image given to the
codec at or after media time `t` as a key frame request; the codec makes that
image a key frame. Under HLS the instance requests one at each grid boundary
([encoder.md §8.4](encoder.md#84-split_encode-frame)). Audio and subtitle
streams ignore it.

## 8. Flush and end

`flush` drains, upstream first ([draining.md §3](draining.md#3-order)):

- **Audio.** The resampler is drained; its output goes through the
  re-framer. The re-framer is drained; its last frame is short when the input
  did not fill it. The short last frame goes to the codec as it is. A codec
  that does not accept a short last frame receives it padded with silence by
  FFmpeg, which records the padding in the packet durations so that a
  container able to signal it does ([draining.md §4](draining.md#4-partial-final-units)).
- **Video.** The current frame-rate converter is drained (§4.6).
- **Subtitle.** Nothing is held.

The codec itself is drained by the container close
([encoder.md §7](encoder.md#7-stop)). `flush` runs at `stop` and before a
container restart ([encoder.md §13.2](encoder.md#132-what-a-restart-does)); a
restart then builds new stream encoders, with new stages and a new codec.
Under HLS the new stream encoder continues its predecessor's sample count or
output frame number, so the new codec's first pts follows the old codec's
last frame (encoder.md §13.2, "Timelines").

### 8.1 Conformance

- One second of 44100 Hz stereo sine encoded to AAC in ADTS: 45 packets, as
  the FFmpeg command line gives.
- One second of 48000 Hz audio encoded to Opus in Ogg: the decoded output,
  trimmed by the container's pre-skip and end trimming, holds 48000 samples.
- A video re-encode whose source changes pixel format mid-stream: no input
  frame is lost at the change.

## 9. Properties

`codec_attr` and `bitrate` come from the stream; `video_size` from the video
stream's codec parameters. All are none before `mk_stream`
([encoder.md §8.6](encoder.md#86-properties)).

## 10. Options and errors

Options: every key of the track's `opts` must be consumed by the codec, the
device context or FFmpeg's generic codec options
([encoder.md §4](encoder.md#4-option-handling)). Errors of §2 are classed
by [encoder.md §5.2](encoder.md#52-data-faults-and-output-faults): a failure
that follows deterministically from the configuration (codec lookup, a
parameter rejected for the configured format), or any creation failure at
the output's first start-up (a hardware device included), is a configuration
error and fails the script. A creation failure at a later rebuild (a
restart, a reopen, an HLS restart, a codec rebuild) is an output fault: the
output restarts under its policy, since hardware session limits and device
resets are not known to be unrecoverable. Faults while encoding follow §6.

## 11. Logs

Messages are examples; levels are binding.

| Level     | Message                                                                               |
| --------- | ------------------------------------------------------------------------------------- |
| severe    | the encoder lookup or stream creation failure (§2), before raising                    |
| warning   | a dropped frame, subtitle or packet, with the field and FFmpeg's error (§6)           |
| info      | a resampler or frame-rate converter rebuilt, with old and new parameters (§4.2, §4.6) |
| important | hardware-configuration selection lines (§2.2)                                         |
