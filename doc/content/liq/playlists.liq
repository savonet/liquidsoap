# Shuffle, play every URI, start over.
s1 = playlist("/my/playlist.txt")

# Do not randomize
s2 = playlist(mode="normal", "/my/pl.m3u")

# The playlist can come from any URI, can be reloaded every 10 minutes.
s3 = playlist(reload=600,"http://my/playlist.txt")
# END
output.dummy(fallible=true, s1)
output.dummy(fallible=true, s2)
output.dummy(fallible=true, s3)
