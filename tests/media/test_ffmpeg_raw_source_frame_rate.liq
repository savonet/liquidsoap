%include "../test.liq"

# Raw video encoded with no explicit framerate keeps the source's rate, here
# 30000/1001 against the internal 25 fps.
fname = argv(default="", 1)
out_file = "test_ffmpeg_raw_source_frame_rate.mp4"

if file.exists(out_file) then file.remove(out_file) end

s = input.ffmpeg(self_sync=false, fname)

def on_close(_) =
  let json.parse (parsed :
    {streams: [{codec_type: string, r_frame_rate: string, nb_frames: string}]}
  ) = test.ffprobe(out_file)

  let [stream] = list.filter(fun (s) -> s.codec_type == "video", parsed.streams)
  frames = int_of_string(stream.nb_frames)
  print(
    "Output video: #{stream.r_frame_rate}, #{frames} frames."
  )

  # 2 s of content: 60 frames at the source rate, 50 at the internal one.
  if
    stream.r_frame_rate == "30000/1001" and frames >= 58
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
      if float(frames()) * settings.frame.duration() >= 2. then o.stop() end
    end
)
