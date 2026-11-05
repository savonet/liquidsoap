# BEGIN
live = input.ffmpeg("srt://0.0.0.0:9000?mode=listener")

output.file.hls(
  "/var/www/hls",
  fallible=true,
  segment_duration=6.,
  [("copy", %ffmpeg(format = "mpegts", %audio.copy, %video.copy))],
  live
)
# END
