%include "../test.liq"

# Raw video from input.ffmpeg sets the frame dimensions when they are not
# configured. The input connects after the encoder is created.
fname = argv(default="", 1)
out_file = "test_ffmpeg_raw_detect_dimensions.mp4"

if file.exists(out_file) then file.remove(out_file) end

s = input.ffmpeg(self_sync=false, fname)

def on_close(_) =
  let json.parse (parsed : {streams: [{width: int, height: int}]}) =
    test.ffprobe(out_file)

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
    out_file,
    (s : source(video=ffmpeg.video.raw))
  )

frames = ref(0)
s.on_frame(
  synchronous=false,
  fun () ->
    begin
      frames := frames() + 1
      if frames() == 25 then o.stop() end
    end
)
