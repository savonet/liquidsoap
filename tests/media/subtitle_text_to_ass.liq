log.level.set(4)

# Test custom text_to_ass function for subtitle encoding

# Use native SRT decoder (not ffmpeg) to get text format subtitles
# text_to_ass is only called for text format, not ASS format
settings.decoder.decoders := ["srt"]

let {video = v} = source.tracks(video.testsrc.ffmpeg(duration=15.))
let {subtitles = sub} = source.tracks(once(single("test-subtitle.srt")))

s = source({video=v, subtitles=sub})
s = once(s)

clock.assign_new(id="test", sync="none", [s])

out = "subtitle_text_to_ass_output.mkv"

# Custom text_to_ass function that adds a recognizable style
def custom_text_to_ass(i, text) =
  "#{i},0,CustomStyle,,0,0,0,,MARKER:#{text}"
end

# Expected ASS dialogue lines with our custom formatting
# The encoder adds timestamps between layer and style from text_to_ass output
expected_ass =
  "Dialogue: 0,0:00:01.00,0:00:03.00,CustomStyle,,0,0,0,,MARKER:First subtitle line
Dialogue: 0,0:00:04.00,0:00:06.00,CustomStyle,,0,0,0,,MARKER:Second subtitle line
Dialogue: 0,0:00:07.00,0:00:09.00,CustomStyle,,0,0,0,,MARKER:Third subtitle line
Dialogue: 0,0:00:10.00,0:00:12.00,CustomStyle,,0,0,0,,MARKER:Fourth subtitle line
Dialogue: 0,0:00:13.00,0:00:15.00,CustomStyle,,0,0,0,,MARKER:Fifth subtitle line"

def on_close(encoded_fname) =
  ass_fname = file.temp("subtitle_text_to_ass", ".ass")
  ignore(
    process.run(
      "ffmpeg -y -i #{process.quote(encoded_fname)} -map 0:s:0 #{
        process.quote(ass_fname)
      }"
    )
  )

  result = file.contents(ass_fname)
  file.remove(ass_fname)

  # Extract just the Dialogue lines for comparison
  result_lines = string.split(separator="\n", result)
  dialogue_lines =
    list.filter(
      fun (line) -> string.contains(substring="Dialogue:", line),
      result_lines
    )
  result_dialogues = string.concat(separator="\n", dialogue_lines)

  if
    string.trim(result_dialogues) == string.trim(expected_ass)
  then
    test.pass()
  else
    print("Expected:\n#{expected_ass}")
    print("Got:\n#{result_dialogues}")
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
    %subtitles(codec = "ass", text_to_ass = custom_text_to_ass)
  ),
  out,
  s
)
