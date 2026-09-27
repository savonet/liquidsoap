radio = mksafe(playlist("~/music"))

output.icecast(
  %mp3,
  host="icecast.example.org",
  password="hackme",
  mount="/radio.mp3",
  send_icy_metadata=true,
  icy_metadata=["song", "title", "artist", "album"],
  icy_song=fun (m) ->
    m["title"] == ""
      ? null
      : "#{m['artist']}: #{m['title']}",
  encoding="ISO-8859-1",
  radio
)
