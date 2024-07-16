#!/usr/bin/liquidsoap
# Log dir
log.file.path.set("/tmp/basic-radio.log")

# Music
myplaylist = playlist("~/radio/music.m3u")

# Some jingles
jingles = playlist("~/radio/jingles.m3u")

# If something goes wrong, we'll play this
security = single("~/radio/sounds/default.ogg")

# Start building the feed with music
radio = myplaylist

# Now add some jingles
radio = random(weights=[1, 4], [jingles, radio])

# And finally the security
radio = fallback(track_sensitive=false, [radio, security])

# Stream it out
output.icecast(
  %vorbis,
  host="localhost",
  port=8000,
  password="hackme",
  mount="basic-radio.ogg",
  radio
)
