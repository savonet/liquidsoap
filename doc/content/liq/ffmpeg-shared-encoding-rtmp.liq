# An audio source
audio = ...

# Encode it in mp3
audio = ffmpeg.encode.audio(
  %ffmpeg(%audio(codec="libmp3lame")),
  audio)

# Send it to icecast
output.icecast(
  %ffmpeg(format="mp3", %audio.copy),
  host = "...",
  password = "...",
  mount = "/stream",
  audio
)

# A video source, for instance a static image
video = ...

# Encode it in h264 format
video = ffmpeg.encode.video(
  %ffmpeg(%video(codec="libx264")),
  video)

# Mux it with the audio
stream = source.mux.video(video=video, audio)

# Copy encoder for the rtmp stream
enc = %ffmpeg(
  format="flv",
  %audio.copy,
  %video.copy)

# Send to YouTube
key = "..."
url = "rtmp://a.rtmp.youtube.com/live2/#{key}"
output.url(url=url, enc, stream)

# Send to Facebook
key = "..."
url = "rtmps://live-api-s.facebook.com:443/rtmp/#{key}"
output.url(self_sync=true, url=url, enc, stream)
