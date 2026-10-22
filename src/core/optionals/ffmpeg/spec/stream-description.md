# FFmpeg stream description

This file owns the description of an opened input container: which streams it
has, which Liquidsoap field each one gets, the one-line description logged for
it, the specific container format name, and the content type the container
yields for a requested content type (`get_type`). It relies on the FFmpeg
bindings to read streams and codec parameters
([bindings spec](../../../../modules/synced/ffmpeg/spec/README.md)) and on the
content kinds and their merge rules of [content.md §3](content.md#3-parameters).
Its callers are the decoder ([decoder.md](decoder.md)) and
`input.harbor.dynamic`'s FFmpeg path ([io.md](io.md)).

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

Data streams have no time base in the model.

## 2. `container ?format ~url input`

1. `format` is the given input format, else the input format the bindings
   report for the opened container (possibly none).
2. Log at info level that the container is being analysed, with the input
   format name when known and the URL. Example (wording not binding):

   ```
   [decoder.ffmpeg.description:3] Analyzing container for format: "mov,mp4,m4a,3gp,3g2,mj2", uri: "test.mp4"
   ```

3. Describe the streams of each medium in container order (§3).
4. If no stream of any medium was described: fail with an error saying that
   the container has no valid stream. This is a source fault of that input
   ([faults.md §3](faults.md#3-source-faults-reach-the-script)): the caller
   does not decode it, and nothing downstream stops.
5. `streams` = audio streams, then video, then subtitle, then data, each in
   container order.
6. `format` = the specific format name (§4) of the input format, or none.

## 3. Fields

Each stream gets a field name from its medium and its rank `n` among **all**
the container's streams of that medium, in container order, counted from 0:

| Medium   | n = 0       | n ≥ 1             |
| -------- | ----------- | ----------------- |
| audio    | `audio`     | `audio_<n+1>`     |
| video    | `video`     | `video_<n+1>`     |
| subtitle | `subtitles` | `subtitles_<n+1>` |
| data     | `data`      | `data_<n+1>`      |

Each medium is ranked on its own; data streams are ranked among data streams
only. The first data stream is `data` whatever the number of subtitle
streams.

Reading a stream's parameters can fail with an FFmpeg error. The stream is
then left out of `streams`, and the failure is logged at warning level with
the medium and the error. This is an item fault
([faults.md §2](faults.md#2-classes)) whose item is the stream. It needs no
escalation bound: a container has finitely many streams, and a container
with none left is a source fault (§2 step 4). The stream keeps its rank: the next stream of that
medium gets the next name, not the failed stream's. The decoder maps fields
to streams with the same ranking
([decoder.md §6](decoder.md#6-stream-selection-and-field-mapping), owner of
the mapping). So a field name in the content type returned by `get_type`
always designates the stream the decoder fills under that name.

Field names beyond the first (`audio_2`, `video_3`, …) are ordinary field
names. A script MAY write them in annotations
(`source(audio_2=ffmpeg.audio.raw)`); the type checker resolves them as it
resolves `audio`. An unknown field name in an annotation is a configuration
error ([faults.md §2](faults.md#2-classes)), never an internal exception. The type checker owns this; the decoder relies
on it.

Conformance:

- A file with streams audio, subtitle, data: fields `audio`, `subtitles`,
  `data`.
- A file with two audio streams whose first fails to describe: the second is
  `audio_2` in `get_type`, and the decoder fills `audio_2` from it.

## 4. Specific format name

The input format name is a comma-separated list for some demuxers. It is
narrowed as follows:

1. Name starts with `mov`: map the container metadata `major_brand`:

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

   An unknown brand is logged at debug level and keeps the name. No
   `major_brand` keeps the name.

2. Name starts with `matroska`: `webm` when the bindings expose the file's
   EBML DocType and it is `webm`, `matroska` when it is `matroska`.
   Otherwise the name is kept.
3. Any other name is kept.

Observed values over common media: `aac`, `flac`, `matroska,webm`, `mp3`,
`mp4`, `ogg`, `wav`.

## 5. `describe`

Each stream gives one string; strings are joined by `, `:

| Medium   | Form                                                                    |
| -------- | ----------------------------------------------------------------------- |
| audio    | `<field>: {codec: <codec_name>, <samplerate>Hz, <channels> channel(s)}` |
| video    | `<field>: {codec: <codec_name>, <width>x<height>, <pixel_format>}`      |
| subtitle | `<field>: {codec: <codec_name>, subtitle}`                              |
| data     | `<field>: {codec: <codec_name>}`                                        |

Example, logged by the decoder (wording not binding):

```
[decoder.ffmpeg:3] FFmpeg recognizes "test.mp4" as audio: {codec: aac, 44100Hz, 2 channel(s)}, video: {codec: h264, 320x240, yuv420p}
```

## 6. `get_type ~ctype container`

Maps the container onto a requested content type `ctype` (field → format).
`ctype` MUST be the consuming source's own content type, not a copy: the
merges below are how a copy track's parameters are set and checked
([content.md §3.1.1](content.md#311-where-copy-parameters-are-merged)).
`get_type` logs the requested content type first and the decoded content
type last, at info level.

The result holds exactly the fields that are both in the container and in
`ctype`. A container field absent from `ctype` is left out; a `ctype` field
absent from the container is left out. Data streams never appear. Every
result field is the requested format `f` of that field, never a new format,
except where the table names one.

Audio streams, for a requested format `f`:

| `f`                | Result field                                                   | Side effect                                                                                                                  |
| ------------------ | -------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `ffmpeg.copy`      | `f`                                                            | `f` is merged with the stream's copy parameters; only type properties can fail ([content.md §3.1](content.md#31-ffmpegcopy)) |
| `ffmpeg.audio.raw` | `f`                                                            | none: the first decoded frame sets it ([content.md §14](content.md#14-raw-audio-conformance))                                |
| any other          | a PCM format of the kind of `f` with the codec's channel count | none                                                                                                                         |

Video streams:

| `f`                | Result field | Side effect                                                                                                         |
| ------------------ | ------------ | ------------------------------------------------------------------------------------------------------------------- |
| `ffmpeg.copy`      | `f`          | `f` is merged with the stream's copy parameters, average frame rate included                                        |
| `ffmpeg.video.raw` | `f`          | none: the first decoded frame sets it ([content.md §9](content.md#9-raw-video-conformance))                         |
| any other          | `f`          | the video format's alpha parameter is set from the codec if still unset ([common.md §7](common.md#7-pixel-formats)) |

Subtitle streams:

| `f`                                                                  | Result field                                                    | Side effect                                                         |
| -------------------------------------------------------------------- | --------------------------------------------------------------- | ------------------------------------------------------------------- |
| `ffmpeg.copy`                                                        | `f`                                                             | `f` is merged with the stream's copy parameters, time base included |
| the internal subtitle format, and the codec is a text subtitle codec | the internal subtitle format                                    | none                                                                |
| any other                                                            | the default internal video format (subtitles rendered as video) | none                                                                |

A copy merge fails only on a type property (codec, bitstream form,
geometry, sample rate, channel layout, and pixel or sample format when both
are set; [content.md §3.1](content.md#31-ffmpegcopy)). Header details and
descriptive properties never fail it: the encoder judges them at the source
switch. A failed copy merge fails `get_type` with the incompatible-format
error of [content.md §3](content.md#3-parameters): it names the property
that differs and prints both parameter sets. The caller does not decode that container
([decoder.md](decoder.md)); a source that was being opened is rejected, and
nothing of it is produced. This is a source fault of that file or
connection ([faults.md §3](faults.md#3-source-faults-reach-the-script)): a
playlist moves on, a reconnecting input that keeps meeting it is not ready
so a `fallback` moves on, and the output keeps streaming.

Merges mutate `f` in place, so the consuming source's content type carries
the container's copy parameters from then on.

Examples, on an AAC/H.264 MP4. Requested
`{audio=ffmpeg.audio.raw,video=ffmpeg.video.raw}` gives the requested
formats unchanged until the first frames:

```
Decoded content-type: {audio=ffmpeg.audio.raw,video=ffmpeg.video.raw}
```

Requested `{audio=ffmpeg.copy,video=ffmpeg.copy}` gives

```
Decoded content-type: {audio=ffmpeg.copy(codec="aac",channel_layout="stereo",sample_format=fltp,sample_rate=44100),video=ffmpeg.copy(codec="h264",width=320,height=240,aspect_ratio=1/1,framerate=25/1,pixel_format=yuv420p)}
```

Conformance:

- `get_type` twice on one copy content type, with an MP4 H.264 file then an
  MPEG-TS H.264 file: the second call fails with the error above (bitstream
  form).
- The same with two MP4 H.264 files that differ only in extradata or in
  their display matrix: both calls succeed.
- `get_type` with a requested `ffmpeg.audio.raw(sample_rate=48000)` on a
  44.1 kHz file returns that requested format; the decoded audio is
  delivered at 48 kHz.

## 7. Script-visible records

`input.harbor.dynamic` exposes each stream as a record built from this model
([io.md](io.md)):

| Medium   | Fields                                                                                                                              |
| -------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| audio    | `field`, `type="audio"`, `codec`, `samplerate`, `channels`, `channel_layout`                                                        |
| video    | `field`, `type="video"`, `codec`, `width`, `height`, `pixel_format`, `frame_rate` (float `num/den`, `null` if unknown or `den ≤ 0`) |
| subtitle | `field`, `type="subtitle"`, `codec`                                                                                                 |
| data     | `field`, `type="data"`, `codec`                                                                                                     |

and the container's specific `format` (or `null`).
