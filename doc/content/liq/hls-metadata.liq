s = mksafe(playlist("playlist"))

output.file.hls(
  "/tmp/path/to/directory",
  [
    ("aac", %ffmpeg(format = "adts", %audio(codec = "aac")).{id3_version=3}),
    (
      "ts-with-meta",
      %ffmpeg(format = "mpegts", %audio(codec = "aac")).{id3_version=4}
    ),
    ("ts", %ffmpeg(format = "mpegts", %audio(codec = "aac")).{id3=false}),
    (
      "mp3",
      %ffmpeg(format = "mp3", %audio(codec = "libmp3lame")).{replay_id3=false}
    )
  ],
  s
)
