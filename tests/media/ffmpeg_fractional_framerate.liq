%include "../test.liq"

log.level.set(5)

out = "ffmpeg_fractional_framerate.ts"
if file.exists(out) then file.remove(out) end

s = once(video.testsrc.ffmpeg(duration=2.))

# Two seconds of 25 fps input retimed to 29.97 fps: 60 frames.
def on_close(_) =
  let json.parse (parsed :
    {streams: [{r_frame_rate: string, nb_read_packets: string}]}
  ) = test.ffprobe(count_packets=true, out)

  let [stream] = parsed.streams
  print(
    "Output frame rate: #{stream.r_frame_rate}, frames: #{
      stream.nb_read_packets
    }"
  )
  if
    stream.r_frame_rate == "30000/1001" and stream.nb_read_packets == "60"
  then
    test.pass()
  else
    test.fail()
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(
    format = "mpegts",
    %video(codec = "libx264", framerate = "30000/1001")
  ),
  out,
  s
)
