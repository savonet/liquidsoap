s = playlist("bla")

# BEGIN
output.file(
  %external(
    process =
      "ffmpeg -i pipe:0 -f avi pipe:1",
    video = true
  ),
  "/tmp/test.avi",
  s
)
