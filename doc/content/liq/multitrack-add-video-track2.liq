# A playlist of audio files
s = playlist("a_playlist")

# A static image
image = single("/path/to/image.png")

# Mux the audio tracks with the image
s = source(source.tracks(s).{video=source.tracks(image).video})

# END
output.dummy(fallible=true, s)
