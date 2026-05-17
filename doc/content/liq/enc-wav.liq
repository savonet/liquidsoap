source = sine()

# BEGIN
# Uncompressed 24-bit stereo archive
output.file(%wav(samplesize = 24), "/tmp/archive.wav", source)

# Raw PCM pipe to external process
output.external(
  %wav(header = false),
  "ffmpeg -f s16le -ar 44100 -ac 2 -i - out.mp4",
  source
)
# END
