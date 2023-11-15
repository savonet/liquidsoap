s = playlist("bla")
output.file(
  %external(
    process =
      "ffmpeg -i pipe:0 -f avi pipe:1",
    video = true
  ),
  "/tmp/test.avi",
  s
)
