radio = mksafe(playlist("~/music"))

# BEGIN
output.file.hls(
  "/var/www/hls",
  [("aac", %fdkaac(bitrate = 128)), ("mp3", %mp3(bitrate = 128))],
  radio
)
# END
