# Input the stream from an Icecast server or any other source
url = "https://icecast.radiofrance.fr/fip-hifi.aac"
input = mksafe(input.http(url))

# First transcoder: mp3 32 kbps. We also degrade the samplerate, and encode in
# mono Accordingly, a mono conversion is performed on the input stream
output.icecast(
  %mp3(bitrate = 32, samplerate = 22050, stereo = false),
  mount="/your-stream-32.mp3",
  host="streaming.example.com",
  port=8000,
  password="xxx",
  mean(input)
)

# Second transcoder: mp3 128 kbps using %ffmpeg
output.icecast(
  %ffmpeg(format = "mp3", %audio(codec = "libmp3lame", b = "128k")),
  mount="/your-stream-128.mp3",
  host="streaming.example.com",
  port=8000,
  password="xxx",
  input
)
