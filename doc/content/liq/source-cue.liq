#!./liquidsoap

radio = sine()
f = radio.insert_metadata

# BEGIN
radio =
  source.cue(
    title=
      "My stream",
    file="backup.mp3",
    "/tmp/backup.cue",
    radio
  )
output.file(%mp3, "/tmp/backup.mp3", radio)

# END
thread.run(
  delay=1.,
  {f([("artist", "artist1"), ("album", "album1"), ("title", "title1")])}
)
thread.run(
  delay=2.,
  {f([("artist", "artist2"), ("album", "album1"), ("title", "title2")])}
)
thread.run(delay=3., shutdown)
