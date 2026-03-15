log.level.set(4)

let {video = v} = source.tracks(video.testsrc.ffmpeg(duration=32.))
let {subtitles = s} = source.tracks(single("test-subtitle.srt"))

s = source({video=v, subtitles=s})

s = once(s)

clock.assign_new(sync="none", [s])

expected_srt =
  "1
00:00:01,000 --> 00:00:03,000
First subtitle line

2
00:00:04,000 --> 00:00:06,000
Second subtitle line

3
00:00:07,000 --> 00:00:09,000
Third subtitle line

4
00:00:10,000 --> 00:00:12,000
Fourth subtitle line

5
00:00:13,000 --> 00:00:15,000
Fifth subtitle line

6
00:00:16,000 --> 00:00:18,000
First subtitle line

7
00:00:19,000 --> 00:00:21,000
Second subtitle line

8
00:00:22,000 --> 00:00:24,000
Third subtitle line

9
00:00:25,000 --> 00:00:27,000
Fourth subtitle line

10
00:00:28,000 --> 00:00:30,000
Fifth subtitle line

11
00:00:31,000 --> 00:00:33,000
First subtitle line"

def on_close(encoded_fname) =
  srt_fname = file.temp("subtitle_concat", ".srt")
  ignore(
    process.run(
      "ffmpeg -y -i #{process.quote(encoded_fname)} -map 0:s:0 #{
        process.quote(srt_fname)
      }"
    )
  )

  result = file.contents(srt_fname)
  file.remove(srt_fname)

  if
    string.trim(result) == string.trim(expected_srt)
  then
    test.pass()
  else
    print("Expected:\n#{expected_srt}")
    print("Got:\n#{result}")
    test.fail()
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  reopen_on_error=fun (_) ->
    begin
      test.fail()
      null
    end,
  %ffmpeg(
    format = "matroska",
    %video(codec = "libx264"),
    %subtitles(codec = "subrip")
  ),
  "subtitles_concat.mkv",
  s
)
