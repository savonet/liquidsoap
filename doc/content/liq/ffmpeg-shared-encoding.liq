# Input the stream,
# from an Icecast server or any other source
source = input.http("https://icecast.radiofrance.fr/fip-hifi.aac")

# Make it infallible:
source = mksafe(source)

# Encode it in mp3:
source = ffmpeg.encode.audio(%ffmpeg(%audio(codec = "libmp3lame")), source)

# Send to one server here:
output.icecast(
  %ffmpeg(format = "mp3", %audio.copy),
  mount="/restream",
  host="streaming.example.com",
  port=8000,
  password="xxx",
  source
)

# An another one here:
output.icecast(
  %ffmpeg(format = "mp3", %audio.copy),
  mount="/restream",
  host="streaming2.example.com",
  port=8000,
  password="xxx",
  source
)
