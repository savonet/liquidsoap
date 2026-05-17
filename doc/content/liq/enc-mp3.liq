source = sine()

# BEGIN
# Standard 128 kbps stereo stream
output.icecast(%mp3(bitrate=128), host="myserver", mount="stream.mp3", source)

# High-quality VBR archive
output.file(%mp3.vbr(quality=0), "/tmp/archive.mp3", source)

# Average bitrate with bounds
output.file(%mp3.abr(bitrate=192, min_bitrate=128, max_bitrate=320), "/tmp/out.mp3", source)

# Low-bitrate mono speech (use mean() to downmix stereo to mono)
output.file(%mp3(bitrate=32, mono=true), "/tmp/speech.mp3", mean(source))
# END
