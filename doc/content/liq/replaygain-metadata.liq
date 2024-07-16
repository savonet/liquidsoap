enable_replaygain_metadata()

s = playlist("~/playlist")
s = amplify(1., override="replaygain_track_gain", s)
# END
output.dummy(fallible=true, s)
