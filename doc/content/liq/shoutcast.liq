source = single("audiofile.ogg")

output.shoutcast(
  %mp3,
  host="shoutcast.example.org",
  port=8000,
  password="changeme",
  source
)
