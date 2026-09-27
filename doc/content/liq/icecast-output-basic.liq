radio = mksafe(playlist("~/music"))

output.icecast(
  %mp3(bitrate = 128),
  host="icecast.example.org",
  port=8000,
  password="hackme",
  mount="/radio.mp3",
  name="My radio",
  description="Music all day long",
  genre="Various",
  url="https://radio.example.org",
  radio
)
