# Internal and raw stream encoders

This file describes the stream encoders that encode content with an FFmpeg
codec: audio and video in `Internal` mode (from Liquidsoap PCM and YUV
frames) and `Raw` mode (from decoded FFmpeg frames carried as
`ffmpeg.audio.raw` / `ffmpeg.video.raw`), and subtitles. It follows the
contract numbering of [encoder.md §12](encoder.md#12-stream-encoder-contract); §§1–4 are the contract's
§12.1–§12.4 and so on. It relies on the raw content layout of
[content.md](content.md), on the bindings' stream creation and
`write_frame` ([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)),
and on the scaling settings of [common.md §10](common.md#10-scaling) and the helper
graphs of [filters.md §16](filters.md#16-helper-graphs). Timestamps are in
[timestamps.md §3](timestamps.md#3-audio-internal-and-raw)–§5.

## 1. Interface

| Media    | `mk_stream`                       | `flush`                                         | `codec_attr`, `bitrate`                  | `video_size`                                                |
| -------- | --------------------------------- | ----------------------------------------------- | ---------------------------------------- | ----------------------------------------------------------- |
| audio    | nothing (stream created at build) | nothing                                         | from the stream                          | none                                                        |
| video    | creates the stream (§2.2)         | end of stream for the frame-rate converter (§8) | from the stream, none before `mk_stream` | codec parameters' width and height, none before `mk_stream` |
| subtitle | nothing                           | nothing                                         | from the stream                          | none                                                        |

## 2. Stream creation

### 2.1 Audio (at build)

1. Encoder lookup by `codec` name. Not found: logged at severe
   (`Cannot find encoder <c>: <exn>.`) and, for FFmpeg's
   encoder-not-found error, raises
   `Cannot find %ffmpeg encoder <c>. Make sure that ffmpeg is compiled with it!`
   (kind `encoder`); any other exception is re-raised.
2. Target parameters:
   - sample rate = the track's `samplerate`;
   - channel layout = FFmpeg's default layout for `channels`; no default
     raises `%ffmpeg encoder: could not find a default channel configuration
for <n> channels..`;
   - sample format = FFmpeg's best sample format of the codec for the
     requested one (`sample_format`, default `dbl`); an unknown name raises
     FFmpeg's error.
3. The stream is added with that sample rate, layout and format, time base
   `1/samplerate`, the codec and a copy of the track options. A failure is
   logged at severe (`Cannot create audio stream (samplerate: …, time_base: …,
channel layout: …, sample format: …, options: …): <exn>.`) and re-raised.
4. Options check ([encoder.md §4](encoder.md#4-option-handling)).
5. `intra_only` = the codec descriptor's intra-only property (true when the
   codec has no descriptor).
6. `frame_size` = none when the codec has the variable-frame-size capability,
   else the stream encoder's frame size.

### 2.2 Video (at `mk_stream`)

At build: encoder lookup (not found: severe log `Cannot find encoder <c>:
<exn>.` and the exception re-raised as is); target frame rate, width and
height forced from the track; the scaling algorithm
([common.md §10](common.md#10-scaling)); an unknown value raises
`Invalid value set for ffmpeg scaling algorithm!`.

At `mk_stream frame`:

1. `alpha` = the track's `alpha` when set, else whether the frame's content
   type for the field is a video format with alpha.
2. Target pixel format: the encoder pixel format of
   [common.md §7](common.md#7-pixel-formats), from the track's `pixel_format` and `alpha`.
3. Hardware context and stream pixel format: [common.md §11](common.md#11-hardware-contexts),
   from `hwaccel`, `hwaccel_device`, `hwaccel_pixel_format`, the track
   options and the target pixel format.
4. Side data: [side-data.md §7](side-data.md#7-side-data-given-to-encoders).
5. The stream is added: time base `1/framerate`, frame rate `framerate/1`,
   the chosen pixel format and hardware context, width and height, a copy of
   the options.
6. Options check ([encoder.md §4](encoder.md#4-option-handling)).
7. `intra_only` as for audio.

Audio and subtitle streams are added and copy streams reserved at build, in
track order. Internal and raw video streams are added at `mk_stream`, which
runs in field iteration order, the reverse of track order
([encoder.md §5](encoder.md#5-encoding-a-frame)). So video streams come after every build-time
stream, and several video tracks come out in reverse track order: with
`%ffmpeg(%video(codec=…), %audio(codec=…))` audio is stream 0 and video
stream 1. The parameters of a video stream come from the first frame.

### 2.3 Subtitle (at build)

Encoder lookup (not found: severe `Cannot find subtitle encoder <c>: <exn>.`,
re-raised). Stream time base `1/1000000`. Default ASS header
([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)).
Options check.

## 3. Input

| Media / mode   | Reads                                                                                                     |
| -------------- | --------------------------------------------------------------------------------------------------------- |
| audio internal | the field's PCM samples, from offset 0 for the frame's audio length (the PCM kind of the track)           |
| audio raw      | every frame in every chunk of the field whose position is in `[0, frame length)`                          |
| video internal | every image of the field's video data, in order                                                           |
| video raw      | every frame in every chunk with position in `[0, frame length)`, with the chunk's time base and source id |
| subtitle       | every subtitle with position in `[0, frame length)`                                                       |

A frame of length 0 is ignored by `encode` (not by `mk_stream`). Chunk
positions are chunk-relative ([content.md](content.md)); the range test uses the
frame length, so it accepts every frame of every chunk.

## 4. Conversion

### 4.1 Audio internal

One resampler per stream, built at build time: from the global audio rate,
FFmpeg's default layout for the track's `channels` (the type checker makes it
the frame's channel count), the PCM kind's planar format, to the target rate,
layout and sample format. Each `encode` converts the frame's samples and yields
one FFmpeg audio frame.

### 4.2 Audio raw

On the first frame received, a resampler is chosen from that frame's rate,
layout and sample format: identity when all three equal the target, else an
FFmpeg-frame to FFmpeg-frame resampler. It is kept for the stream's lifetime;
later frames with other parameters go through the same resampler
([findings](findings/encoder.md)).

### 4.3 Re-framing

When `frame_size` is set (§2.1), converted frames go through an audio
re-framer that delivers frames of exactly `frame_size` samples
([avfilter.md](../../../../modules/synced/ffmpeg/spec/avfilter.md), audio
converter). It is never told end of stream (§8).

### 4.4 Video internal

One scaler, built on the first image: from the global video size and
`yuva420p`/`yuv420p` (alpha when the target pixel format has alpha) to the
target size and pixel format, with the scaling threads of
[common.md §10](common.md#10-scaling). Each image is first cut to the global video
size (viewport) and rendered onto an opaque canvas unless `alpha`, then packed
and scaled.

### 4.5 Video raw

A frame of the target width and height passes as is. Any other is fitted:
scaled into the target size keeping its proportions, square target pixels,
padded, same pixel format, timestamps kept ([filters.md §16.4](filters.md#164-fit)).

### 4.6 Frame-rate conversion (both video modes)

Every video frame then goes through a frame-rate converter keyed on (pixel
format, time base, source id). A frame whose key differs from the current
converter's builds a new one; the old one is dropped without being drained.
The converter is the Fps helper graph of [filters.md §16.2](filters.md#162-fps); its
timestamp rules are in [timestamps.md §4](timestamps.md#4-video-internal-and-raw).

### 4.7 Subtitle

Each subtitle becomes one FFmpeg subtitle with one ASS rectangle:

- ASS text = the subtitle's text when its format is ASS, else
  `text_to_ass(n, text)` where `n` counts the subtitles of this stream from 0;
- flags = `forced` when the subtitle is forced;
- start and end display times: [timestamps.md §5](timestamps.md#5-subtitles).

## 5. Timestamps

### 5.1 Audio

[timestamps.md §3](timestamps.md#3-audio-internal-and-raw). The same audio frame writer (timestamping,
optional re-framing, then `write_frame`) is used by the inline encoders.

### 5.2 Video

[timestamps.md §4](timestamps.md#4-video-internal-and-raw).

### 5.3 Subtitle

[timestamps.md §5](timestamps.md#5-subtitles).

## 6. Writing

Audio and video: the bindings' `write_frame`, which encodes and writes every
packet produced, converting from the encoder's time base to the stream's.
Subtitles: `write_subtitle_frame`. Errors propagate; for audio the writer
creation is wrapped to log `Error writing audio frame: <exn>.` at severe, and
re-raise.

## 7. Keyframes

The keyframe hook ([encoder.md §8.2](encoder.md#82-keyframe-hooks)) is passed to `write_frame`;
before each key packet the muxer is flushed unless the codec is intra-only,
then the hook runs. An intra-only audio codec makes every packet a key packet.
Subtitles have no hook.

## 8. Flush and end

- Audio: `flush` does nothing. The re-framer's last partial frame and the
  resampler's delay are not pushed; the codec itself is drained by the
  container close ([encoder.md §7](encoder.md#7-stop);
  [findings](findings/encoder.md)).
- Video: `flush` ends the current frame-rate converter's stream and writes
  what it delivers; frames held by converters dropped at a key change (§4.6)
  are lost.
- Subtitle: nothing.

`flush` runs at `stop` and before an Ogg restart.

## 9. Properties

Audio and subtitle report `codec_attr` and `bitrate` from the stream from
build. Video reports them, and `video_size` from the codec parameters, only
after `mk_stream`. Before the first frame an HLS output asking for video
codec strings gets none.

## 10. Options and errors

Options: every key of the track's `opts` must be consumed by the codec, the
device context or FFmpeg's generic options. Errors listed in §2; FFmpeg
errors from encoding propagate from `encode`.

An unknown PCM kind on the track is an internal error (`Invalid`).

## 11. Logs

| Level     | Message                                          |
| --------- | ------------------------------------------------ |
| severe    | `Cannot find encoder <c>: <exn>.` (audio, video) |
| severe    | `Cannot find subtitle encoder <c>: <exn>.`       |
| severe    | `Cannot create audio stream (…): <exn>.`         |
| severe    | `Error writing audio frame: <exn>.`              |
| important | hardware-configuration selection lines (§2.2)    |
