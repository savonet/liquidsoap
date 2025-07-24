# A fallback switch
s = fallback([playlist("http://my/playlist"), single("/my/jingle.ogg")])

# END
output.dummy(fallible=true, s)
