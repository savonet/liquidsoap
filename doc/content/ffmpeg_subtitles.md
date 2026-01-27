# FFmpeg Subtitles

FFmpeg provides advanced subtitle support including encoding, decoding from media containers, and copying encoded subtitle streams. For general subtitle concepts, see [Subtitles](subtitles.html).

## Decoding from media files

Subtitles can be decoded from media files using `input.ffmpeg`:

```liquidsoap
let {video, subtitle} = source.tracks(
  input.ffmpeg("input.mkv")
)
```

Decoding only works with text-based subtitle codecs (SubRip, ASS/SSA, WebVTT, MOV text). Bitmap-based subtitles (DVD, PGS, DVB) can only be copied.

## Encoding subtitles

The `%subtitle` encoder converts subtitle content into encoded subtitle streams:

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

When encoding, text subtitles are converted to ASS format. You can customize this conversion:

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

The `text_to_ass` function receives the read order index and text, returning an ASS dialogue line.

## Copying subtitles

The `%subtitle.copy` encoder passes through encoded subtitle data without re-encoding. This works with both text-based and bitmap-based subtitle codecs:

```liquidsoap
let {subtitle} = source.tracks(
  input.ffmpeg(self_sync=true, "input.mkv")
)

output.file(
  %ffmpeg(format="matroska", %video(codec="libx264"), %subtitle.copy),
  "output.mkv",
  source({video=video, subtitle=subtitle})
)
```

## Multiple subtitle tracks

Multiple subtitle tracks can be encoded using numbered stream names:

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

You can combine `%subtitle.copy` with `%subtitle` encoding in the same output:

```liquidsoap
# Raw subtitle stream for copy
let {subtitle = sub_copy} = source.tracks(
  input.ffmpeg(self_sync=true, "input.mkv")
)

# Decoded subtitle for encoding
let {subtitle = sub_encode} = source.tracks(
  (single("additional.srt"):source(audio=none,video=none))
)

s = source({video=video, subtitle=sub_copy, subtitle_2=sub_encode})

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

Text-based subtitles from an existing media file can be decoded and re-encoded to a different format:

```liquidsoap
let {video, subtitle} = source.tracks(
  input.ffmpeg("input.mkv")
)

output.file(
  %ffmpeg(format="matroska", %video.copy, %subtitle(codec="ass")),
  "output.mkv",
  source({video=video, subtitle=subtitle})
)
```
