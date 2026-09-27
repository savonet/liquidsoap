# Multitrack

Liquidsoap lets you work with the individual tracks of a source. You can
extract the audio, video, metadata and track marks of a source, process each of
them separately and combine them into a new source. For instance, you can keep
the audio tracks of several languages from a movie file, remux streams without
re-encoding them, or apply different processing to audio and video.

## What is a track?

A source produces a _frame_ on each streaming cycle. A frame is a collection of
typed fields, typically `audio`, `video`, `metadata` and `track_marks`. A frame
can have any number of named audio or video fields, for instance `audio_2`.
Liquidsoap calls each field a _track_.

The type of a source describes its tracks. For example:

```liquidsoap
source(audio=pcm(stereo), video=yuv420p)
```

describes a source with a stereo PCM audio track and a YUV420P video track.

## Content types depend on usage

The tracks of a source depend on how you use the source. Liquidsoap uses type
inference to determine the content type of each source: the operators that
consume a source determine the content that the source must produce.

For instance, if you write:

```liquidsoap
s = single("movie.mkv")
output.file(%ffmpeg(%audio.copy, %video.copy), "/path/to/copy.mkv", s)
```

then the encoder tells Liquidsoap that it needs `audio` and `video` in FFmpeg
copy format. The source `s` then asks the decoder for exactly those tracks.

The decoder only decodes the tracks that you request. For a file with three
audio tracks, the script above only produces `audio`. To get the other audio
tracks, request `audio_2` and `audio_3` explicitly.

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

Each extracted value is a _track_ tied to the underlying source. You can then
combine tracks into a new source:

```liquidsoap
s = source({audio = audio, video = video, metadata = metadata, track_marks = track_marks})
```

You can also replace one track and keep the others:

```liquidsoap
image = single("logo.png")
s = source(source.tracks(s).{video = source.tracks(image).video})
```

To drop a track, use the `_` pattern or the `source.drop.*` operators:

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
Requests to the decoder must use these names. For instance, to keep both audio
tracks of a file and re-encode the second one to stereo AAC:

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

When the source is a playlist, it only accepts files that contain all the
requested tracks. It skips the files that miss one of them.

Once you extract tracks with `source.tracks`, you can give them any name when
building a new source.

## Track-level operators

Many processing operators work on tracks. This lets you apply different
processing to each track:

```liquidsoap
let {audio, video} = source.tracks(s)

# Convert to mono
mono = track.audio.mean(audio)

# Encode audio to AAC via FFmpeg
encoded = track.ffmpeg.encode.audio(%ffmpeg(%audio(codec="aac")), audio)
```

### Clocks and cross-clock composition

Some track operators, in particular the FFmpeg encoders, place their input in a
new clock. This matters when you combine the output of such an operator with
other tracks.

All tracks of a source must belong to the same clock. When you rebuild a source
from an encoded track, take its `metadata` and `track_marks` from the encoded
track. `track.metadata` and `track.track_marks` return the metadata and track
marks associated with any track:

```liquidsoap
let {audio} = source.tracks(s)

encoded = track.ffmpeg.encode.audio(%ffmpeg(%audio(codec="aac")), audio)

# Take metadata and track_marks from the encoded track,
# which belongs to the encoder's clock
s = source({
  audio = encoded,
  metadata = track.metadata(encoded),
  track_marks = track.track_marks(encoded)
})
```

## Inspecting content types at runtime

### source.content

`source.content` returns the content type of a source as an associative list
mapping field names to their format values:

```liquidsoap
s = (noise() : source(audio=pcm, video=yuv420p))

def print_content(entry) =
  let (field, fmt) = entry
  print("#{field}: #{format.description(fmt)}")
end

list.iter(print_content, source.content(s))
```

The returned list is the content that the type checker has determined for the
source. As explained above, this content depends on how the source is used.

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

The methods are optional (marked `?`) because a given format only has one of
them. Use safe navigation to access them:

```liquidsoap
fmt = track.format(audio)
desc = format.description(fmt)

if null.defined(desc?.pcm) then
  pcm = null.get(desc?.pcm)
  print("PCM: #{pcm.channels} channels, layout: #{pcm.channel_layout}")
end
```

## Encoder track type hints

When you use custom track names in an FFmpeg encoder, Liquidsoap needs to know
whether each track is audio, video or subtitles. Copy tracks, such as
`%en.copy`, need no type. For other tracks, Liquidsoap uses, in priority order:

1. An explicit `audio_content`, `video_content` or `subtitle_content` hint in the encoder parameters
2. The track name containing `"subtitle"`, `"audio"` or `"video"`
3. The media type of the codec, when `codec` is a constant string

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

FFmpeg does not receive the Liquidsoap track names. In the output file, the
tracks are numbered streams, in the order in which they appear in the encoder.
