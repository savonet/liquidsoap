source = sine()

# BEGIN
# Standard Ogg/Vorbis stream
output.icecast(%vorbis(quality=0.5), host="myserver", mount="stream.ogg", source)

# High-quality archive
output.file(%vorbis(quality=0.9), "/tmp/archive.ogg", source)
# END
