# Multitrack

Liquidsoap supports working with individual tracks inside a source. Audio, video, metadata and track marks can each be extracted,
manipulated independently, and recombined into a new source. This unlocks
workflows like keeping multiple language audio tracks, remuxing streams without
re-encoding, or applying different processing chains to audio and video.

## What is a track?

A source produces a _frame_ on each streaming cycle. A frame is a collection of
typed fields — typically `audio`, `video`, `metadata` and `track_marks`, though
you can have any number of named audio or video fields (e.g. `audio_2`). Each
field is what Liquidsoap calls a _track_.

The type of a source describes its tracks. For example:

```liquidsoap
source(audio=pcm(stereo), video=yuv420p)
```

describes a source with a stereo PCM audio track and a YUV420P video track.

## Content types drive what you get

An important subtlety: **what tracks a source exposes depends on how you use
it**. Liquidsoap uses type inference to determine the content type of each
source. The expected content is inferred from the downstream operators and
propagated back to the source.

For instance, if you write:

```liquidsoap
s = single("movie.mkv")
output.file(%ffmpeg(%audio.copy, %video.copy), "/path/to/copy.mkv", s)
```

then the encoder tells Liquidsoap that it needs `audio` and `video` in FFmpeg
copy format. That requirement propagates back to `s`, which then instructs the
decoder to provide exactly those tracks.

If you do not request a track, the decoder will not decode it. A file with three
audio tracks will only produce `audio` (and discard `audio_2`, `audio_3`) unless
you explicitly ask for them.

You can force a particular content type with a type annotation:

```liquidsoap
s = (single("movie.mkv") : source(audio=pcm(stereo), video=yuv420p))
```

## Demuxing and remuxing tracks

Use `source.tracks` to split a source into its individual tracks:

```liquidsoap
s = single("movie.mkv")
let {audio, video, metadata, track_marks} = source.tracks(s)
```

Each extracted value is a _track_ — a typed handle tied to the underlying
source. You can then recombine tracks into a new source:

```liquidsoap
s = source({audio = audio, video = video, metadata = metadata, track_marks = track_marks})
```

Or replace one track while keeping the others:

```liquidsoap
image = single("logo.png")
s = source(source.tracks(s).{video = source.tracks(image).video})
```

To drop a track entirely, use the `_` pattern or the dedicated `source.drop.*`
operators:

```liquidsoap
# Drop track_marks by pattern
let {track_marks = _, ...tracks} = source.tracks(s)
s = source(tracks)

# Or equivalently
s = source.drop.track_marks(s)
```

## Track naming conventions

When decoding a file with multiple tracks of the same type, the decoder names
them `audio`, `audio_2`, `audio_3`, ... and `video`, `video_2`, `video_3`, ...
These names are assigned by the decoder and cannot be changed at the input side.
You can however give them any name when remuxing:

```liquidsoap
output.file(
  %ffmpeg(
    %audio.copy,
    %audio_2(channels=2, codec="aac"),
    %video.copy
  ),
  "/path/to/copy.mkv",
  s
)
```

Only files containing all requested tracks will be accepted when using a
playlist. Files missing any of the required tracks will be skipped.

## Track-level operators

Many processing operators work directly on tracks rather than full sources. This
lets you apply different processing to each track independently:

```liquidsoap
let {audio, video} = source.tracks(s)

# Convert to mono
mono = track.audio.mean(audio)

# Encode audio to AAC via FFmpeg
encoded = track.ffmpeg.encode.audio(%ffmpeg(%audio(codec="aac")), audio)
```

### Clocks and cross-clock composition

Some track operators — in particular FFmpeg encoders — assign the track to a
new clock. This becomes relevant when you want to recombine encoded tracks with
other tracks derived from a different source.

When rebuilding a source from tracks that live on different clocks, you also
need to re-derive `metadata` and `track_marks` from a track on the same clock.
`track.metadata` and `track.track_marks` extract those special tracks from any
content track, making it straightforward to reassemble a full source:

```liquidsoap
let {audio} = source.tracks(s)

encoded = track.ffmpeg.encode.audio(%ffmpeg(%audio(codec="aac")), audio)

# Re-derive metadata and track_marks from the encoded track,
# which now lives on the encoder's clock
s = source({
  audio = encoded,
  metadata = track.metadata(encoded),
  track_marks = track.track_marks(encoded)
})
```

This pattern is the standard way to rebuild a complete source on the encoder's
clock. All tracks in a source must share the same clock, so `metadata` and
`track_marks` must also be derived from the encoded track rather than from the
original source.

## Inspecting content types at runtime

### source.content

`source.content` returns the content type of a source as an associative list
mapping field names to their format values:

```liquidsoap
s = (noise() : source(audio=pcm, video=yuv420p))

list.iter(
  fun (entry) ->
    let (field, fmt) = entry
    print("#{field}: #{format.description(fmt)}"),
  source.content(s)
)
```

The returned list reflects what the type checker has determined the source will
produce — which, as explained above, depends on how the source is used.

### track.format

`track.format` returns the content format of a single track:

```liquidsoap
let {audio, video} = source.tracks(s)

print("audio format: #{format.description(track.format(audio))}")
print("video format: #{format.description(track.format(video))}")
```

### format.description

`format.description` converts a `content_format` value into a record with one
optional method per content type. Its full type is:

```
(content_format) -> {
  ffmpeg_copy? : string,
  ffmpeg_raw_audio? : string,
  ffmpeg_raw_video? : string,
  metadata? : string,
  midi? : {channels : int},
  pcm? : {channel_layout : string, channels : int},
  pcm_f32? : {channel_layout : string, channels : int},
  pcm_s16? : {channel_layout : string, channels : int},
  subtitle? : string,
  track_marks? : string,
  yuv420p? : {height : int, width : int}
}
```

The main content types and their fields are:

| Format           | Method              | Fields                         |
| ---------------- | ------------------- | ------------------------------ |
| `pcm`            | `pcm?`              | `{ channels, channel_layout }` |
| `pcm_s16`        | `pcm_s16?`          | `{ channels, channel_layout }` |
| `pcm_f32`        | `pcm_f32?`          | `{ channels, channel_layout }` |
| `yuv420p`        | `yuv420p?`          | `{ width, height }`            |
| `midi`           | `midi?`             | `{ channels }`                 |
| FFmpeg copy      | `ffmpeg_copy?`      | string description             |
| FFmpeg raw audio | `ffmpeg_raw_audio?` | string description             |
| FFmpeg raw video | `ffmpeg_raw_video?` | string description             |

Methods are optional (marked `?`) because a given format will only populate one
of them. Use safe navigation to access them:

```liquidsoap
fmt = track.format(audio)
desc = format.description(fmt)

if null.defined(desc?.pcm) then
  pcm = null.get(desc?.pcm)
  print("PCM: #{pcm.channels} channels, layout: #{pcm.channel_layout}")
end
```

## Encoder track type hints

When writing an FFmpeg encoder with custom track names, Liquidsoap needs to know
whether each track is audio or video. It determines this from, in priority order:

1. `%audio.copy` or `%video.copy` — type is inferred from the format
2. An explicit `audio_content` or `video_content` hint in the encoder spec
3. The track name containing `"audio"` or `"video"` as a substring
4. The codec name implying a type

For full control, use explicit hints:

```liquidsoap
output.file(
  %ffmpeg(
    %en(audio_content, codec=audio_codec),
    %director_cut(video_content, codec=video_codec)
  ),
  "/path/to/output.mkv",
  s
)
```

Note that once tracks are handed to FFmpeg for muxing, their Liquidsoap names
are lost — FFmpeg sees them as numbered streams in the order they appear.
