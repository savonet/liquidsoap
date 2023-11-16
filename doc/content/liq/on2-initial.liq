# The file source
songs = playlist("/path/to/some/files/")

# The jingle source
jingles = playlist("/path/to/some/jingles")

# We combine the sources and play
# one single every 3 songs:
s = rotate(weights=[1,3], [jingles, songs])

# We output the stream to an icecast
# server, in ogg/vorbis format.
output.icecast(%vorbis,id="icecast",
               fallible=true,mount="my_radio.ogg",
               host="my_server", password="hack_me_not",
               s)
