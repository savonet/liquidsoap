live = input.pulseaudio()
interlude = single("/path/to/sorryfortheblank.ogg")

# After 5 sec of blank the microphone stream is ignored, which causes the stream
# to fallback to interlude.  As soon as noise comes back to the microphone the
# stream comes back to the live -- thanks to track_sensitive=false.
stream =
  fallback(track_sensitive=false, [blank.strip(max_blank=5., live), interlude])

# Put that stream to a local file
output.file(%vorbis, "/tmp/hop.ogg", stream)
