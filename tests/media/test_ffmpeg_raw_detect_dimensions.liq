%include "../test.liq"

# Raw video from input.ffmpeg sets the frame dimensions when they are not
# configured (GH5662). The input connects after the encoder is created.
fname = argv(default="", 1)
out = "test_ffmpeg_raw_detect_dimensions.mp4"

if file.exists(out) then file.remove(out) end

s = input.ffmpeg(self_sync=false, fname)
s = (s : source(video=ffmpeg.video.raw))

def on_close(_) =
  let json.parse (parsed : {streams: [{width: int, height: int}]}) =
    test.ffprobe(out)

  let [stream] = parsed.streams
  print(
    "Output video size is #{stream.width}x#{stream.height}."
  )
  if
    stream.width == 640 and stream.height == 360
  then
    test.pass()
  else
    test.fail()
  end
end

o =
  output.file(
    fallible=true,
    on_close=on_close,
    %ffmpeg(format = "mp4", %video.raw(codec = "libx264")),
    out,
    s
  )

thread.run(delay=1., o.stop)
