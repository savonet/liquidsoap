#!/usr/bin/liquidsoap

set outputs = [ "radio.ogg" ]
set radio.ogg.host = "localhost"
# The default behavior, which will be used by radio.ogg,
# is to connect to an icecast2 server on localhost,
# and stream high-quality Ogg/vorbis.

# The scheduler plays a randomized playlist, or the default.ogg file
# if the playlist fails to stream anything. The playlist can be made
# of remote URIs.
add_w [
# Comment out the next four lines if you don't have speech synthesis
# enabled.
       ((delay 10.
         (one_file
            "say:The Savonet team thanks you for using liquidsoap!
             We hope you'll enjoy it.")),5);
       ((fallback [
         (playlist "/your/playlist.pls") ;
         (one_file "/some/default.ogg")]),1)]
