s = playlist("a_playlist")

# A default video source:
image = single("/path/to/image.png")

# Pick `s` video track if it has one, otherwise use the default one:
video = source.tracks(s).video ?? source.tracks(image).video

# Return a source that always has video:
s = source(source.tracks(s).{video=video})
output.dummy(fallible=true, s)output.dummy(image)
