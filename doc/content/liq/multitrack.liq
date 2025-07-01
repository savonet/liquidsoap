s = single("/path/to/movie.mkv")

# Copy first audio track and video:
output.file(%ffmpeg(%audio.copy, %video.copy), "/path/to/copy.mkv", s)
