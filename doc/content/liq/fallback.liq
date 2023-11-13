# A fallback switch
s = fallback([playlist("http://my/playlist"), single("/my/jingle.ogg")])
output.dummy(fallible=true, s)
