radio = mksafe(playlist("~/music"))

# BEGIN
output.harbor.hls(
  port=8080,
  path="/radio/",
  [("aac", %fdkaac(bitrate = 128)), ("mp3", %mp3(bitrate = 128))],
  radio
)
# END
