s = single("/path/to/movie.mkv")

# Copy first audio track and video track
# and re-encode second audio track:
output.file(
  %ffmpeg(%audio.copy, %audio_2(channels = 2, codec = "aac"), %video.copy),
  "/path/to/copy.mkv",
  s
)
