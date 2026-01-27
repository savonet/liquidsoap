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

For decoding subtitles from media containers, see [FFmpeg Subtitles](ffmpeg_subtitles.html).

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
  end
end, s)
```

The callback receives the same record as `on_subtitle` and returns:

- A record with optional fields `text`, `format`, `forced` to update specific properties
- An empty record `{}` to keep the subtitle unchanged
- `null` to remove the subtitle

Only fields that are returned will be updated:

```liquidsoap
s = subtitles.map(fun (sub) ->
  # Only change format, keep text and forced unchanged
  {format="ass"}
end, s)
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

## FFmpeg integration

For advanced subtitle handling including encoding to various formats, copying encoded subtitles, and decoding from media containers, see [FFmpeg Subtitles](ffmpeg_subtitles.html).
