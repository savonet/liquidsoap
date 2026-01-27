log.level.set(4)

let {video = v} = source.tracks(video.testsrc.ffmpeg(duration=16.))

# FFmpeg raw subtitle for copy mode
let {subtitles = sub_copy} = source.tracks(input.ffmpeg("test-subtitle.srt"))

# SRT decoded subtitle for encoding
let {subtitles = sub_encode} = source.tracks(single("test-subtitle.srt"))

s = source({video=v, subtitles=sub_copy, subtitles_2=sub_encode})

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
    %subtitles.copy,
    %subtitles_2(codec = "subrip")
  ),
  "subtitle_copy_encode.mkv",
  s
)
