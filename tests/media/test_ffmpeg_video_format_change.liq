%include "../test.liq"

log.level.set(4)
settings.decoder.decoders.set(["ffmpeg"])

# One second of 320x240 followed by one second of 240x320, in one stream.
s = once(single("changing.ts"))

def on_close(fname) =
  let json.parse (parsed : {streams: [{duration: string}]}) =
    test.ffprobe(fname)

  let [stream] = parsed.streams
  duration = float_of_string(stream.duration)

  # The change of size starts a new track, which ends `once`.
  if 0.5 < duration and duration < 1.5 then test.pass() else test.fail() end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(format = "mp4", %video(codec = "mpeg4")),
  "video-format-change.mp4",
  s
)
