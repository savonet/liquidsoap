radio = mksafe(playlist("~/music"))

output.icecast(
  %mp3(bitrate = 128),
  host="icecast.example.org",
  password="hackme",
  mount="/radio.mp3",
  radio
)

output.icecast(
  %vorbis(quality = 0.5),
  host="icecast.example.org",
  password="hackme",
  mount="/radio.ogg",
  radio
)

output.icecast(
  %fdkaac(bitrate = 64),
  host="icecast.example.org",
  password="hackme",
  mount="/radio.aac",
  radio
)
