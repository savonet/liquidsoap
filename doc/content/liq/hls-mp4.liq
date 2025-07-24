radio = mksafe(playlist("playlist"))

aac_lofi =
  %ffmpeg(
    format = "mp4",
    %audio(codec = "aac", channels = 2, ar = 44100, b = "192k")
  )

flac_hifi =
  %ffmpeg(
    format = "mp4",
    strict = "-2",
    %audio(codec = "flac", channels = 2, ar = 44100)
  )

flac_hires =
  %ffmpeg(
    format = "mp4",
    strict = "-2",
    %audio(codec = "flac", channels = 2, ar = 48000)
  )

streams =
  [("aac_lofi", aac_lofi), ("flac_hifi", flac_hifi), ("flac_hires", flac_hires)]

output.file.hls(playlist="live.m3u8", "/tmp/path/to/directory", streams, radio)
