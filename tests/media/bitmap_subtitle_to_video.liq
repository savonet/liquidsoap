log.level.set(4)

settings.frame.video.width := 720
settings.frame.video.height := 480

fname = argv(default="", 1)
out = "bitmap_subtitle_to_video_output.mp4"

s = single(fallible=true, fname)
s = once(s)

let {audio = a, video = v, subtitles = sub} = source.tracks(s)

# Convert bitmap subtitles to video by overlaying on the video track
s = source({audio=a, video=track.video.add([v, sub])})

clock.assign_new(id="test", sync="none", [s])

def on_close(encoded_fname) =
  process.run("sync")

  # Verify the output has audio and video streams (no subtitles - they're burned in)
  ojson =
    process.read(
      "ffprobe -v quiet -print_format json -show_streams #{
        process.quote(encoded_fname)
      }"
    )

  let json.parse (parsed : {streams: [{codec_type: string}]}) = ojson

  has_audio = list.exists(fun (s) -> s.codec_type == "audio", parsed.streams)
  has_video = list.exists(fun (s) -> s.codec_type == "video", parsed.streams)
  has_subtitle =
    list.exists(fun (s) -> s.codec_type == "subtitle", parsed.streams)

  if
    has_audio and has_video and not has_subtitle
  then
    test.pass()
  else
    print(
      "Expected audio and video streams with no subtitle stream"
    )
    print(
      "Has audio: #{has_audio}, has video: #{has_video}, has subtitle: #{
        has_subtitle
      }"
    )
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
  %ffmpeg(%audio(codec = "aac"), %video(codec = "libx264")),
  out,
  s
)
