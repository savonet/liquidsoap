# FFmpeg stream description

This file owns the description of an opened input container: which streams it
has, which Liquidsoap field each one gets, the one-line description logged for
it, the specific container format name, and the content type the container
yields for a requested content type (`get_type`). It relies on the FFmpeg
bindings to read streams and codec parameters
([bindings spec](../../../../modules/synced/ffmpeg/spec/README.md)) and on the
content kinds and their merge rules in [content.md](content.md). Its callers
are the decoder ([decoder.md](decoder.md)) and `input.harbor.dynamic`'s FFmpeg
path ([io.md](io.md)).

## 1. Model

```
container = { format : string option; streams : stream list }
stream    = { field : field; params : stream_params }
stream_params =
  | Audio    { codec_name; codec_params; time_base; samplerate; channels; channel_layout }
  | Video    { codec_name; codec_params; time_base; width; height; pixel_format; frame_rate }
  | Subtitle { codec_name; codec_params; time_base }
  | Data     { codec_name; codec_params }
```

| Field             | Source                                                                                                                    |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------- |
| `codec_name`      | codec id of the codec parameters, as a string (bindings)                                                                  |
| `codec_params`    | the stream's codec parameters, kept for `get_type`                                                                        |
| `time_base`       | the stream's time base                                                                                                    |
| `samplerate`      | codec parameters' sample rate                                                                                             |
| `channels`        | codec parameters' channel count                                                                                           |
| `channel_layout`  | description of the codec parameters' channel layout (e.g. `stereo`, `mono`)                                               |
| `width`, `height` | codec parameters                                                                                                          |
| `pixel_format`    | name of the codec parameters' pixel format; `none` if the format has no name; `unknown` if the codec parameters have none |
| `frame_rate`      | the stream's average frame rate, if known                                                                                 |

Data streams have no time base.

## 2. `container ?format ~url input`

1. `format` is the given input format, else the input format the bindings
   report for the opened container (possibly none).
2. Log at level 3 (important), logger `decoder.ffmpeg.description`:
   `Analyzing container for format: "<input format name>", uri: "<url>"`, or
   `Analyzing container for "<url>"` with no format. Strings are quoted with
   the language's string quoting.
3. Collect streams per media, each in container order (§3).
4. If no stream of any media was collected: raise
   `Failure "No valid stream found in container."`.
5. `streams` = audio streams, then video, then subtitle, then data, each in
   container order.
6. `format` = the specific format name (§4) of the input format, or none.

Example (from the implementation, an MP4 with AAC and H.264):

```
[decoder.ffmpeg.description:3] Analyzing container for format: "mov,mp4,m4a,3gp,3g2,mj2", uri: "/var/tmp/claude-ffmpeg-spec/content/test.mp4"
```

## 3. Fields

Streams of each media are numbered from 0 among the streams of that media that
were described successfully. Field names:

| Media    | n = 0       | n ≥ 1             |
| -------- | ----------- | ----------------- |
| audio    | `audio`     | `audio_<n+1>`     |
| video    | `video`     | `video_<n+1>`     |
| subtitle | `subtitles` | `subtitles_<n+1>` |
| data     | `data`      | `data_<n+1>`      |

For data streams, `n` is the stream's rank among data streams **plus the
number of subtitle streams**: with one subtitle stream, the first data stream
is `data_2`.

Reading a stream's parameters can raise an FFmpeg error. The stream is then
skipped: the exception is logged (with backtrace) on the description logger
as `Failed to get <audio|video|subtitle|data> stream info: <exn>`, and the
next stream of that media takes the skipped stream's number.

## 4. Specific format name

The input format name is a comma-separated list for some demuxers. It is
narrowed as follows:

1. Name starts with `matroska`: the container metadata `profile` if present,
   else the name unchanged.
2. Name starts with `mov`: map the container metadata `major_brand`:

   | `major_brand`                                                                               | Format |
   | ------------------------------------------------------------------------------------------- | ------ |
   | `isom` `iso2` `iso3` `iso4` `iso5` `iso6` `mp41` `mp42` `avc1` `dash`                       | `mp4`  |
   | `M4A ` `M4A`                                                                                | `m4a`  |
   | `M4V ` `M4V` `M4VP`                                                                         | `m4v`  |
   | `qt  ` `qt`                                                                                 | `mov`  |
   | `3gp4` … `3gp9`                                                                             | `3gp`  |
   | `3g2a` `3g2b` `3g2c`                                                                        | `3g2`  |
   | `mj2s` `mjp2`                                                                               | `mj2`  |
   | `f4v ` `f4v`                                                                                | `f4v`  |
   | `MSNV` (Sony PSP)                                                                           | `mp4`  |
   | `NDAS` `NDSC` `NDSH` `NDSM` `NDSP` `NDSS` `NDXC` `NDXH` `NDXM` `NDXP` `NDXS` (Nero Digital) | `mp4`  |

   An unknown brand logs at level 5 (debug)
   `Unknown major_brand "<brand>", keeping full format list` and keeps the
   name; no `major_brand` keeps the name.

3. Any other name is kept.

Observed values over the in-tree media fixtures: `aac`, `flac`,
`matroska,webm`, `mp3`, `mp4`, `ogg`, `wav`.

## 5. `describe`

Each stream gives one string; strings are joined by `, `:

| Media    | Form                                                                    |
| -------- | ----------------------------------------------------------------------- |
| audio    | `<field>: {codec: <codec_name>, <samplerate>Hz, <channels> channel(s)}` |
| video    | `<field>: {codec: <codec_name>, <width>x<height>, <pixel_format>}`      |
| subtitle | `<field>: {codec: <codec_name>, subtitle}`                              |
| data     | `<field>: {codec: <codec_name>}`                                        |

Example (from the implementation, logged by the decoder):

```
[decoder.ffmpeg:3] FFmpeg recognizes "/var/tmp/claude-ffmpeg-spec/content/test.mp4" as audio: {codec: aac, 44100Hz, 2 channel(s)}, video: {codec: h264, 320x240, yuv420p}
```

## 6. `get_type ~ctype container`

Maps the container onto a requested content type `ctype` (field → format).
Logs at level 3 `Requested content-type: <ctype>` first and
`Decoded content-type: <result>` last.

The result starts empty and holds only fields that are both in the container
and in `ctype`. A container field absent from `ctype` is left out; a `ctype`
field absent from the container is left out. Data streams never appear.

Audio streams, for a requested format `f` on the stream's field:

| `f`                | Result field                                                                             | Side effect                                                                                                                                                                   |
| ------------------ | ---------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ffmpeg.copy`      | `f`                                                                                      | `f` is merged with copy params `` `Audio codec_params `` ([content.md, ffmpeg.copy](content.md#31-ffmpegcopy)); a failed merge raises `Incompatible_format` out of `get_type` |
| `ffmpeg.audio.raw` | a new raw audio format holding the codec's channel layout, sample format and sample rate | `f` is merged with it; if the merge raises and `compatible(f, new)` holds, the error is ignored; if not compatible the error propagates                                       |
| any other          | a new PCM format of the kind of `f` with the codec's channel count                       | none; `f` is not consulted beyond its kind                                                                                                                                    |

Video streams:

| `f`                | Result field | Side effect                                                                                                                           |
| ------------------ | ------------ | ------------------------------------------------------------------------------------------------------------------------------------- |
| `ffmpeg.copy`      | `f`          | merged with `` `Video { avg_frame_rate = frame_rate; codec_params } ``; failure propagates                                            |
| `ffmpeg.video.raw` | `f`          | none: the first decoded frame sets it ([content.md, Raw video conformance](content.md#9-raw-video-conformance))                       |
| any other          | `f`          | the video format's alpha parameter is set from the codec if still unset ([common.md §7](common.md#7-pixel-formats), Set format alpha) |

Subtitle streams:

| `f`                                                                                                                    | Result field                                                    | Side effect                                                                 |
| ---------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------- | --------------------------------------------------------------------------- |
| `ffmpeg.copy`                                                                                                          | `f`                                                             | merged with `` `Subtitle { time_base; codec_params } ``; failure propagates |
| the internal subtitle format, and the codec is a text subtitle codec (codec descriptor has the text-subtitle property) | the internal subtitle format                                    | none                                                                        |
| any other                                                                                                              | the default internal video format (subtitles rendered as video) | none                                                                        |

Merges mutate `f` in place ([content.md, Parameters](content.md#3-parameters)), so the
requested content type itself now carries the container's parameters.

Example (from the implementation): requested
`{audio=ffmpeg.audio.raw,video=ffmpeg.video.raw}` on the AAC/H.264 MP4 gives

```
Decoded content-type: {audio=ffmpeg.audio.raw(sample_rate=44100,sample_format=fltp,channel_layout=stereo),video=ffmpeg.video.raw}
```

and requested `{audio=ffmpeg.copy,video=ffmpeg.copy}` gives

```
Decoded content-type: {audio=ffmpeg.copy(codec="aac",channel_layout="stereo",sample_format=fltp,sample_rate=44100),video=ffmpeg.copy(codec="h264",width=320,height=240,aspect_ratio=1/1,framerate=25/1,pixel_format=yuv420p)}
```

## 7. Script-visible records

`input.harbor.dynamic` exposes each stream as a record built from this model
([io.md](io.md)):

| Media    | Fields                                                                                                                              |
| -------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| audio    | `field`, `type="audio"`, `codec`, `samplerate`, `channels`, `channel_layout`                                                        |
| video    | `field`, `type="video"`, `codec`, `width`, `height`, `pixel_format`, `frame_rate` (float `num/den`, `null` if unknown or `den ≤ 0`) |
| subtitle | `field`, `type="subtitle"`, `codec`                                                                                                 |
| data     | `field`, `type="data"`, `codec`                                                                                                     |

and the container's specific `format` (or `null`).
