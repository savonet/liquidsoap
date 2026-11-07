camera = mksafe(single("/path/to/video.mp4"))

# BEGIN
sd =
  %ffmpeg(
    format = "mpegts",
    %audio(codec = "aac", b = "96k", channels = 2),
    %video(codec = "libx264", width = 640, height = 360, b = "800k")
  )

hd =
  %ffmpeg(
    format = "mpegts",
    %audio(codec = "aac", b = "128k", channels = 2),
    %video(codec = "libx264", width = 1280, height = 720, b = "2500k")
  ).{bandwidth = 3200000, codecs = "avc1.64001f,mp4a.40.2"}

output.file.hls("/var/www/hls", [("sd", sd), ("hd", hd)], camera)
# END
