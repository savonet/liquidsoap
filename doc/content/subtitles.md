# Subtitles

Liquidsoap supports subtitle tracks as a dedicated content type. Subtitles can be decoded, processed, and encoded alongside audio and video streams.

## Subtitle content

Each subtitle entry contains:

- `start_time`: Start time in seconds
- `end_time`: End time in seconds
- `text`: The subtitle text content
- `format`: Either `"ass"` (ASS dialogue format) or `"text"` (plain text)
- `forced`: Whether this is a forced subtitle

## Decoding subtitles

SubRip (.srt) files are natively supported by all builds of liquidsoap:

```liquidsoap
let {subtitle} = source.tracks(single("subtitles.srt"))
```

For decoding subtitles from media containers, see [FFmpeg Subtitles](ffmpeg_subtitles.html).

## Subtitle callbacks

You can react to subtitle events using `track.on_subtitle` (track-level) or `on_subtitle` (source-level):

```liquidsoap
s = on_subtitle(fun (sub) ->
  print("Subtitle [#{sub.start_time}-#{sub.end_time}]: #{sub.text}")
end, s)
```

The callback receives a record with the subtitle fields described above.

## Multiple subtitle tracks

Multiple subtitle tracks can be combined in a single source:

```liquidsoap
let {subtitle = english} = source.tracks(single("english.srt"))
let {subtitle = french} = source.tracks(single("french.srt"))

s = source({video=video, subtitle=english, subtitle_2=french})
```

## Concatenating subtitles

Subtitle sources can be concatenated using `sequence`:

```liquidsoap
let {subtitle = s1} = source.tracks(single("part1.srt"))
let {subtitle = s2} = source.tracks(single("part2.srt"))

subtitle_source = sequence([source({subtitle=s1}), source({subtitle=s2})])
let {subtitle} = source.tracks(subtitle_source)
```

## FFmpeg integration

For advanced subtitle handling including encoding to various formats, copying encoded subtitles, and decoding from media containers, see [FFmpeg Subtitles](ffmpeg_subtitles.html).
