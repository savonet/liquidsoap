s = single("video.mp4")

output.icecast(
  %ffmpeg(
    format = "ogg",
    %audio(codec = "libvorbis"),
    %video(codec = "libtheora")
  ),
  host="localhost",
  port=8000,
  password="hackme",
  mount="/videostream",
  s
)
