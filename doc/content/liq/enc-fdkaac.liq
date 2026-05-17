source = sine()

# BEGIN
# HE-AAC v2 stream at 48 kbps — good quality at low bitrate
output.icecast(
  %fdkaac(bitrate = 48, aot = "mpeg4_he_aac_v2"),
  host="myserver",
  mount="stream.aac",
  source
)

# AAC-LC for Icecast, ADTS muxing
output.file(
  %fdkaac(bitrate = 192, aot = "mpeg4_aac_lc", transmux = "adts"),
  "/tmp/out.aac",
  source
)

# Variable bitrate, high quality
output.file(%fdkaac(vbr = 5, aot = "mpeg4_aac_lc"), "/tmp/archive.aac", source)
# END
