# Serveur settings
settings.harbor.bind_addrs := ["0.0.0.0"]

# An emergency file
emergency = single("/path/to/emergency/single.ogg")

# A playlist
playlist = playlist("/path/to/playlist")

# A live source
live = input.harbor("live", port=8080, password="hackme")

# fallback
radio = fallback(track_sensitive=false, [live, playlist, emergency])

# output it
output.icecast(%vorbis, mount="test", host="host", radio)
