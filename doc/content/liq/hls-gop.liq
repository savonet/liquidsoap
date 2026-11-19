s = playlist("~/videos")

# BEGIN
# 29.97 fps, 6 s segments: a key frame every 60 frames (2.002 s), three per
# segment, in every variant.
output.file.hls(
  "/var/www/hls",
  segment_duration=6.,
  [
    (
      "720p",
      %ffmpeg(
        format = "mpegts",
        %audio(codec = "aac"),
        %video(
          codec = "libx264",
          width = 1280,
          height = 720,
          g = 60,
          keyint_min = 60,
          sc_threshold = 0
        )
      )
    ),
    (
      "360p",
      %ffmpeg(
        format = "mpegts",
        %audio(codec = "aac"),
        %video(
          codec = "libx264",
          width = 640,
          height = 360,
          g = 60,
          keyint_min = 60,
          sc_threshold = 0
        )
      )
    )
  ],
  mksafe(s)
)
# END
