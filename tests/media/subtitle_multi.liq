log.level.set(4)

let {video = v} = source.tracks(video.testsrc.ffmpeg(duration=16.))
let {subtitles = s1} = source.tracks(single("test-subtitle.srt"))
let {subtitles = s2} = source.tracks(single("test-subtitle.srt"))

s = source({video=v, subtitles=s1, subtitles_2=s2})

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
    %subtitles(codec = "subrip"),
    %subtitles_2(codec = "ass")
  ),
  "subtitle_multi.mkv",
  s
)
