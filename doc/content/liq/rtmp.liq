s = playlist("...")

enc = %ffmpeg(
  format="flv",
  listen=1,
  %audio.copy,
  %video.copy
)

output.url(url="rtmp://host/app/instance", enc, s)
