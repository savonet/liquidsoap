#
# A simple radio script to get started
# Filenames need to be edited in playlist() and single() sources
#

message =
  "The Savonet team thanks you for using liquidsoap, " ^
  "and we hope you'll enjoy it!"

# Then we setup our icecast output function.
# Possible options and values are bitrate=64, samplerate=11025, ...
out = output.icecast(%vorbis,host="localhost")

out(mount="demo.ogg",
  # The scheduler plays a randomized playlist, or the .ogg file
  # if the playlist fails to stream anything. The playlist can be made
  # of remote URIs.
  add([
    # Comment out the next line if you don't have speech synthesis enabled.
    delay(10.,single("say:"^message)),
    # We add another output without the synthesized messages. It is not started
    # but you can turn it on using the server:
    # > telnet localhost 1234
    # > novoice.ogg.start
    out(start=false,mount="novoice.ogg",
        fallback([ playlist("/your/favorite.m3u"),
                   # The out(..) needs the single(..) to be infallible,
                   # which requires a valid audio filename.
                   single("/some/file.ogg") ]))
    ],
    weights=[5,1]))
