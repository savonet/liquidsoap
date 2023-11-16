s = blank()
s2 = blank()
# BEGIN
s2 = video.scale(scale=0.2, x=10, y=10, s2)
s = add([s, s2])
# END
output.dummy(fallible=true, s)
