log.level.set(4)

# First, create an MKV file with subtitles
let {video} = source.tracks(video.testsrc.ffmpeg(duration=16.))
let {subtitle = sub} =
  source.tracks((single("test-subtitle.srt") : source(audio=none, video=none, subtitle=subtitle(text))))

input_source = source({video=video, subtitle=sub})
input_source = once(input_source)

clock.assign_new(id="encode", sync="none", [input_source])

def on_close(_) =
  # Now re-encode subtitles from the generated MKV
  let {video = v2} = source.tracks(input.ffmpeg("subtitle_reencode_input.mkv"))
  let {subtitle = sub2} =
    source.tracks(input.ffmpeg("subtitle_reencode_input.mkv"))

  output_source = source({video=v2, subtitle=sub2})
  output_source = once(output_source)

  clock.assign_new(id="re-encode", sync="none", [output_source])

  output.file(
    fallible=true,
    on_close=fun (_) -> test.pass(),
    reopen_on_error=fun (_) ->
      begin
        test.fail()
        null
      end,
    %ffmpeg(format = "matroska", %video.copy, %subtitle(codec = "ass")),
    "subtitle_reencode_output.mkv",
    output_source
  )
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
    %subtitle(codec = "subrip")
  ),
  "subtitle_reencode_input.mkv",
  input_source
)
