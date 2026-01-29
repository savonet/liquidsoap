log.level.set(4)

let {video} = source.tracks(video.testsrc.ffmpeg(duration=16.))

# FFmpeg raw subtitle for copy mode
let {subtitle = sub_copy} =
  source.tracks(input.ffmpeg(self_sync=true, "test-subtitle.srt"))

# SRT decoded subtitle for encoding
let {subtitle = sub_encode} =
  source.tracks(
    (single("test-subtitle.srt") :
      source(audio=none, video=none, subtitle=subtitle(text))
    )
  )

s = source({video=video, subtitle=sub_copy, subtitle_2=sub_encode})

s = once(s)

clock.assign_new(sync="none", [s])

output.file(
  fallible=true,
  on_close=fun (_) -> test.pass(),
  reopen_on_error=fun (_) ->
    begin
      test.fail()
      null
    end,
  %ffmpeg(
    format = "matroska",
    %video(codec = "libx264"),
    %subtitle.copy,
    %subtitle_2(codec = "subrip")
  ),
  "subtitle_copy_encode.mkv",
  s
)
