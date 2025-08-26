#!/usr/bin/liquidsoap

# Lines starting with # are comments, they are ignored.

# Put the log file in some directory where you have permission to write.
log.file.path := "/tmp/<script>.log"

# Print log messages to the console, can also be done by passing the -v option
# to Liquidsoap.
log.stdout := true

# Use the telnet server for requests
settings.server.telnet := true

# A bunch of files and playlists, supposedly all located in the same base dir.
default = single("~/radio/default.ogg")

day = playlist("~/radio/day.pls")
night = playlist("~/radio/night.pls")
jingles = playlist("~/radio/jingles.pls")
clock = single("~/radio/clock.ogg")

# Play user requests if there are any, otherwise one of our playlists, and the
# default file if anything goes wrong.
radio =
  fallback(
    [
      request.queue(id="request"),
      switch([({6h-22h}, day), ({22h-6h}, night)]),
      default
    ]
  )

# Add the normal jingles
radio = random(weights=[1, 5], [jingles, radio])

# And the clock jingle
radio = add([radio, switch([({0m0s}, clock)])])

radio = mksafe(radio)

# Add the ability to relay live shows
full =
  fallback(
    track_sensitive=false,
    [input.http("http://localhost:8000/live.ogg"), radio]
  )

# Output the full stream in OGG and MP3
output.icecast(
  %mp3,
  host="localhost",
  port=8000,
  password="hackme",
  mount="radio",
  full
)
output.icecast(
  %vorbis,
  host="localhost",
  port=8000,
  password="hackme",
  mount="radio.ogg",
  full
)

# Output the stream without live in OGG
output.icecast(
  %vorbis,
  host="localhost",
  port=8000,
  password="hackme",
  mount="radio_nolive.ogg",
  radio
)
