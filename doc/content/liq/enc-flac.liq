source = sine()

# BEGIN
# Lossless archive
output.file(%flac(compression=8, bits_per_sample=24), "/tmp/archive.flac", source)

# Streamable Ogg/FLAC
output.icecast(%ogg(%flac), host="myserver", mount="stream.flac", source)
# END
