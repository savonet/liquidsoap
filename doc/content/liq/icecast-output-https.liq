radio = mksafe(playlist("~/music"))

output.icecast(
  %mp3,
  transport=http.transport.ssl(),
  host="icecast.example.org",
  port=8443,
  password="hackme",
  mount="/radio.mp3",
  radio
)
