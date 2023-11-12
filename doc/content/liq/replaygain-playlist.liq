enable_replaygain_metadata()

s = replaygain(playlist("~/playlist"))
output.dummy(fallible=true, s)
