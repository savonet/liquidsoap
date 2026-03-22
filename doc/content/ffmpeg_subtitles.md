# FFmpeg Subtitles

FFmpeg provides subtitle support including encoding, decoding from media containers, and copying encoded subtitle streams. For general subtitle concepts, see [Subtitles](subtitles.html).

## Decoding from media files

Subtitles can be decoded from media files using `input.ffmpeg`:

```liquidsoap
let {video, subtitles} = source.tracks(
  input.ffmpeg("input.mkv")
)
```

Decoding is supported only for text-based subtitle codecs (SubRip, ASS/SSA, WebVTT, MOV text). Bitmap-based subtitles (DVD, PGS, DVB) can only be copied.

## Encoding subtitles

The `%subtitle` encoder converts subtitle content into encoded subtitle streams. Example:

```liquidsoap
%ffmpeg(
  format="matroska",
  %video(codec="libx264"),
  %subtitle(codec="subrip")
)
```

Supported codecs depend on the container format:

- **Matroska (.mkv)**: `subrip`, `ass`
- **WebM**: `webvtt`
- **MP4**: `mov_text`

### Custom text to ASS conversion

When encoding, text subtitles are converted to ASS format. To customize this conversion:

```liquidsoap
%ffmpeg(
  format="matroska",
  %video(codec="libx264"),
  %subtitle(
    codec="ass",
    text_to_ass=fun (i, text) ->
      "#{i},0,MyStyle,,0,0,0,,#{text}"
    end
  )
)
```

The `text_to_ass` function takes a read-order index and text, and returns an ASS dialogue line.

## Copying subtitles

The `%subtitle.copy` encoder passes encoded subtitle data through without re-encoding. It is the only way to handle bitmap-based subtitles (DVD, PGS, DVB) and is also efficient for text-based subtitles when no modification is needed.

### Basic copy

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
  source({audio, video, subtitles})
)
```

### Copying all streams from a file

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

### Copying bitmap subtitles

Bitmap-based subtitle formats (DVD/VOBSUB, PGS/Blu-ray, DVB) are stored as images and can only be copied, not decoded or re-encoded:

```liquidsoap
# DVD subtitles from an MKV file
s = input.ffmpeg(self_sync=true, "movie_with_dvd_subs.mkv")

let {video, subtitles} = source.tracks(s)

# Copy the bitmap subtitles to a new container
output.file(
  %ffmpeg(format="matroska", %video.copy, %subtitles.copy),
  "output.mkv",
  source({video, subtitles})
)
```

## Multiple subtitle tracks

Multiple subtitle tracks are encoded using numbered stream names:

```liquidsoap
output.file(
  %ffmpeg(
    format="matroska",
    %video(codec="libx264"),
    %subtitle(codec="subrip"),
    %subtitle_2(codec="ass")
  ),
  "output.mkv", s
)
```

## Mixing copy and encode

`%subtitle.copy` and `%subtitle` encoding can be combined in the same output:

```liquidsoap
# Raw subtitle stream for copy
let {subtitles = sub_copy} = source.tracks(
  input.ffmpeg(self_sync=true, "input.mkv")
)

# Decoded subtitle for encoding
let {subtitles = sub_encode} = source.tracks(
  (single("additional.srt"):source(audio=none,video=none))
)

s = source({video=video, subtitles=sub_copy, subtitles_2=sub_encode})

output.file(
  %ffmpeg(
    format="matroska",
    %video(codec="libx264"),
    %subtitle.copy,
    %subtitle_2(codec="subrip")
  ),
  "output.mkv", s
)
```

## Re-encoding subtitles

Text-based subtitles from a media file can be decoded and re-encoded to a different format:

```liquidsoap
let {video, subtitles} = source.tracks(
  input.ffmpeg("input.mkv")
)

output.file(
  %ffmpeg(format="matroska", %video.copy, %subtitle(codec="ass")),
  "output.mkv",
  source({video=video, subtitles=subtitles})
)
```

## Burning bitmap subtitles into video

Bitmap-based subtitles (DVD, PGS, DVB) cannot be decoded to text, but can be burned into the video track using `track.video.add`. This overlays the subtitle images directly onto the video frames.

This is useful when:

- The target format does not support bitmap subtitles
- Subtitles must always be visible (hardcoded)
- The target player does not support the original subtitle format

### Basic example

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
s = source({audio, video=track.video.add([video, subtitles])})

output.file(
  fallible=true,
  %ffmpeg(%audio(codec="aac"), %video(codec="libx264")),
  "output.mp4",
  s
)
```

### How it works

`track.video.add` takes a list of video-compatible tracks and composites them in order. When a subtitle track is included, each subtitle image is rendered at its designated position and timing, permanently embedding the subtitles in the video.

The output contains:

- Video with subtitles burned in
- Audio (if present)
- No separate subtitle track

### Notes

- Video dimensions should match the source for correct subtitle positioning.
- Burning subtitles is irreversible — the original text cannot be recovered.
- Re-encoding the video is required, which uses more CPU than copying.
- Text-based subtitles can also be burned in, but keeping them as a separate track is generally preferable for flexibility.
