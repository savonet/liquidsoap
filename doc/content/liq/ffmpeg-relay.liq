# Input the stream,
# from an Icecast server or any other source
encoded_source = input.http("https://icecast.radiofrance.fr/fip-hifi.aac")

# Send to one server here:
output.icecast(
  %ffmpeg(format = "adts", %audio.copy),
  fallible=true,
  mount="/restream",
  host="streaming.example.com",
  port=8000,
  password="xxx",
  encoded_source
)

# An another one here:
output.icecast(
  %ffmpeg(format = "adts", %audio.copy),
  fallible=true,
  mount="/restream",
  host="streaming2.example.com",
  port=8000,
  password="xxx",
  encoded_source
)
