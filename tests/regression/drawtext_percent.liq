%include "../test.liq"

# video.add_text.ffmpeg used to fail on a literal `%`: drawtext expanded it as
# the start of a `%{...}` sequence and produced no frames.

log.level := 4
settings.frame.video.width := 160
settings.frame.video.height := 120
settings.frame.video.framerate := 10

frames = ref(0)
text =
  ref(
    "50% done"
  )

s = source.mux.audio(audio=sine(), video.testsrc.ffmpeg(duration=3.))
s = video.add_text.ffmpeg(speed=0, text, s)

# Change the text halfway through, so that the update command is exercised too.
def on_frame() =
  frames := frames() + 1
  if
    s.time() > 1.5
  then
    text :=
      "100% {done}: it's 'ok' \\ really"
  end
end

s.on_frame(synchronous=true, on_frame)

clock.assign_new(sync="none", [s])

def on_close(filename) =
  info =
    json.parse(
      default=[("streams", [[("nb_read_packets", "0")]])],
      test.ffprobe(count_packets=true, filename)
    )

  streams = list.assoc(default=[], "streams", info)
  decoded =
    list.fold(
      fun (n, stream) ->
        if
          list.assoc(default="", "codec_type", stream) == "video"
        then
          n + int_of_string(list.assoc(default="0", "nb_read_packets", stream))
        else
          n
        end,
      0,
      streams
    )

  print(
    "frames seen: #{frames()}, packets: #{decoded}"
  )
  if decoded >= 25 then test.pass() else test.fail() end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(
    format = "mkv",
    %audio(codec = "aac"),
    %video(codec = "libx264", preset = "ultrafast")
  ),
  file.temp(cleanup=true, "drawtext_percent", ".mkv"),
  s
)
