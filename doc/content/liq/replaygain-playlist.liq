enable_replaygain_metadata()

s = replaygain(playlist("~/playlist"))

# END
output.dummy(fallible=true, s)
