frame.duration := 0.1

tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

s = video.testsrc.ffmpeg(duration=10.)

s = mksafe(s)

enc =
  %ffmpeg(
    format = "mpegts",
    %video(
      codec = "libx264",
      maxrate = "4000k",
      bufsize = "10000k",
      preset = "ultrafast",
      tune = "zerolatency",
      x264opts = "keyint=12:min-keyint=12"
    )
  ).{bandwidth=4000000}

streams = [("radio", enc)]

output.file.hls(
  segment_duration=2.0,
  on_file_change=(fun (~state:_, _) -> test.pass()),
  tmp_dir,
  streams,
  s
)
