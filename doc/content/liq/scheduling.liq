# A fallback switch
fallback([playlist("http://my/playlist"),
          single("/my/jingle.ogg")])

# A scheduler,
# assuming you have defined the night and day sources
switch([ ({0h-7h}, night), ({7h-24h}, day) ])
