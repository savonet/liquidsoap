# Subtitles

Liquidsoap supports subtitle tracks as a dedicated content type. Subtitles can be decoded, processed, and encoded alongside audio and video streams.

## Subtitle content

Each subtitle entry contains:

- `position`: Position in the content, in main ticks
- `start_time`: Start time relative to position, in main ticks
- `end_time`: End time relative to position, in main ticks
- `text`: The subtitle text content
- `format`: Either `"ass"` (ASS dialogue format) or `"text"` (plain text)
- `forced`: Whether this is a forced subtitle

Times are stored relative to position to enable proper concatenation of subtitle content.

## Decoding subtitles

SubRip (.srt) files are natively supported by all builds of liquidsoap:

```liquidsoap
let {subtitles} = source.tracks(single("subtitles.srt"))
```

To decode subtitles from media containers such as `.mkv` files, see [Decoding from media files](#decoding-from-media-files).

## Subtitle callbacks

You can react to subtitle events using `track.on_subtitle` (track-level) or `on_subtitle` (source-level):

```liquidsoap
s = on_subtitle(fun (sub) ->
  print("Subtitle at #{sub.absolute_start_time}s: #{sub.text}"), s)
```

The callback receives a record with:

- `position`: Position in the content (main ticks)
- `start_time`: Start time relative to position (main ticks)
- `end_time`: End time relative to position (main ticks)
- `absolute_start_time`: Absolute start time in seconds
- `absolute_end_time`: Absolute end time in seconds
- `text`: Subtitle text content
- `format`: `"ass"` or `"text"`
- `forced`: Whether this is a forced subtitle

## Transforming subtitles

Use `track.subtitles.map` (track-level) or `subtitles.map` (source-level) to transform or filter subtitles:

```liquidsoap
s = subtitles.map(fun (sub) ->
  if sub.text == "" then
    # Remove empty subtitles
    null
  else
    # Modify the text
    {text="[#{sub.format}] #{sub.text}"}
  end, s)
```

The callback receives the same record as `on_subtitle` and returns:

- A record with optional fields `text`, `format`, `forced` to update specific properties
- An empty record `{}` to keep the subtitle unchanged
- `null` to remove the subtitle

Only fields that are returned will be updated:

```liquidsoap
# Only change format, keep text and forced unchanged
s = subtitles.map(fun (_) -> {format="ass"}, s)
```

## Inserting subtitles

Use `track.subtitles.insert` (track-level) or `subtitles.insert` (source-level) to dynamically insert subtitles. The operator returns a track/source with an `insert_subtitle` method:

```liquidsoap
s = subtitles.insert(s)

# Insert a subtitle after 1 second
thread.run(delay=1., {
  s.insert_subtitle({
    duration=5.0,
    text="Hello, world!",
    format="text",
    forced=false
  })
})
```

The `insert_subtitle` method takes a record with:

- `duration`: Duration in seconds
- `text`: Subtitle text content
- `format`: `"ass"` or `"text"`
- `forced`: Whether this is a forced subtitle

The subtitle will be inserted at the current playback position with `start_time=0` and `end_time` set to the specified duration.

If the source doesn't have a subtitle track, `subtitles.insert` will create one.

## Multiple subtitle tracks

Multiple subtitle tracks can be combined in a single source:

```liquidsoap
let {subtitles = english} = source.tracks(single("english.srt"))
let {subtitles = french} = source.tracks(single("french.srt"))

s = source({video=video, subtitles=english, subtitles_2=french})
```

## Concatenating subtitles

Subtitle sources can be concatenated using `sequence`. Times are stored relative to position, so concatenation works correctly:

```liquidsoap
let {subtitles = s1} = source.tracks(single("part1.srt"))
let {subtitles = s2} = source.tracks(single("part2.srt"))

subtitles_source = sequence([source({subtitles=s1}), source({subtitles=s2})])
let {subtitles} = source.tracks(subtitles_source)
```

## FFmpeg subtitles

With FFmpeg support, you can decode subtitles from media files, encode subtitles into a container, copy encoded subtitle streams and burn bitmap subtitles into the video. See [Enabling ffmpeg support](./ffmpeg.md#enabling-ffmpeg-support) to add FFmpeg to your build.

### Decoding from media files

Subtitles can be decoded from media files using `input.ffmpeg`:

```liquidsoap
let {video, subtitles} = source.tracks(
  input.ffmpeg("input.mkv")
)
```

Text-based subtitle codecs (SubRip, ASS/SSA, WebVTT, MOV text) are decoded to text. Bitmap-based subtitles (DVD, PGS, DVB) are images: you can copy them or [burn them into the video](#burning-bitmap-subtitles-into-video).

### Encoding subtitles

The `%subtitles` encoder converts subtitle content into encoded subtitle streams. Example:

```liquidsoap
%ffmpeg(
  format="matroska",
  %video(codec="libx264"),
  %subtitles(codec="subrip")
)
```

Supported codecs depend on the container format:

- **Matroska (.mkv)**: `subrip`, `ass`
- **WebM**: `webvtt`
- **MP4**: `mov_text`

#### Custom text to ASS conversion

When encoding, text subtitles are converted to ASS format. To customize this conversion:

```liquidsoap
%ffmpeg(
  format="matroska",
  %video(codec="libx264"),
  %subtitles(
    codec="ass",
    text_to_ass=fun (i, text) -> "#{i},0,MyStyle,,0,0,0,,#{text}"
  )
)
```

The `text_to_ass` function takes a read-order index and the subtitle text, and returns an ASS dialogue line without its timestamps. The encoder adds the timestamps. The default function returns `"#{i},0,Default,,0,0,0,,#{text}"`.

### Copying subtitles

The `%subtitles.copy` encoder passes encoded subtitle data through as is. Use it to keep bitmap-based subtitles (DVD, PGS, DVB) as a subtitle track, or to pass text-based subtitles that you do not need to modify.

#### Basic copy

```liquidsoap
s = input.ffmpeg(self_sync=true, "input.mkv")

let {audio, video, subtitles} = source.tracks(s)

output.file(
  %ffmpeg(
    format="matroska",
    %audio.copy,
    %video.copy,
    %subtitles.copy
  ),
  "output.mkv",
  source({audio=audio, video=video, subtitles=subtitles})
)
```

#### Copying all streams from a file

To remux a file with all streams intact:

```liquidsoap
s = input.ffmpeg(self_sync=true, "input.mkv")
s = once(s)

output.file(
  fallible=true,
  %ffmpeg(format="matroska", %audio.copy, %video.copy, %subtitles.copy),
  "output.mkv",
  s
)
```

#### Copying bitmap subtitles

Bitmap-based subtitle formats (DVD/VOBSUB, PGS/Blu-ray, DVB) are stored as images. To keep them as a subtitle track, copy them:

```liquidsoap
# DVD subtitles from an MKV file
s = input.ffmpeg(self_sync=true, "movie_with_dvd_subs.mkv")

let {video, subtitles} = source.tracks(s)

# Copy the bitmap subtitles to a new container
output.file(
  %ffmpeg(format="matroska", %video.copy, %subtitles.copy),
  "output.mkv",
  source({video=video, subtitles=subtitles})
)
```

### Encoding multiple subtitle tracks

To encode several subtitle tracks, such as the ones built in [Multiple subtitle tracks](#multiple-subtitle-tracks), use numbered track names in the encoder:

```liquidsoap
output.file(
  %ffmpeg(
    format="matroska",
    %video(codec="libx264"),
    %subtitles(codec="subrip"),
    %subtitles_2(codec="ass")
  ),
  "output.mkv", s
)
```

### Mixing copy and encode

`%subtitles.copy` and `%subtitles` encoding can be combined in the same output:

```liquidsoap
# Encoded subtitle track for copy
let {video, subtitles = sub_copy} = source.tracks(
  input.ffmpeg(self_sync=true, "input.mkv")
)

# Decoded subtitle track for encoding
let {subtitles = sub_encode} = source.tracks(single("additional.srt"))

s = source({video=video, subtitles=sub_copy, subtitles_2=sub_encode})

output.file(
  %ffmpeg(
    format="matroska",
    %video(codec="libx264"),
    %subtitles.copy,
    %subtitles_2(codec="subrip")
  ),
  "output.mkv", s
)
```

### Re-encoding subtitles

Text-based subtitles from a media file can be decoded and re-encoded to a different format:

```liquidsoap
let {video, subtitles} = source.tracks(
  input.ffmpeg("input.mkv")
)

output.file(
  %ffmpeg(format="matroska", %video.copy, %subtitles(codec="ass")),
  "output.mkv",
  source({video=video, subtitles=subtitles})
)
```

### Burning bitmap subtitles into video

Bitmap-based subtitles (DVD, PGS, DVB) can be burned into the video track using `track.video.add`. This draws the subtitle images onto the video frames.

Burn subtitles in when the target format or player does not support the original subtitle format, or when the subtitles must always be visible.

#### Basic example

```liquidsoap
# Optional: set video dimensions to match the source (DVD is typically 720x480 or 720x576)
# This is only needed in cases where video frame size auto-detection does not work.
# settings.frame.video.width := 720
# settings.frame.video.height := 480

s = single("movie_with_dvd_subs.mkv")
s = once(s)

let {audio, video, subtitles} = source.tracks(s)

# Overlay subtitles onto video using track.video.add
# The subtitles track is converted to video and composited on top
s = source({audio=audio, video=track.video.add([video, subtitles])})

output.file(
  fallible=true,
  %ffmpeg(%audio(codec="aac"), %video(codec="libx264")),
  "output.mp4",
  s
)
```

Here, the `subtitles` track is used as a video track, so the decoder renders each subtitle image as a video frame at its position and display time. `track.video.add` then draws the tracks of its list on top of each other, in order. The output file contains the audio and the video with the subtitles burned in, and no subtitle track.

The video dimensions must match the source for the subtitles to be positioned correctly. Burning subtitles requires re-encoding the video, which uses more CPU than copying it.
