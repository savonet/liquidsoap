# BEGIN
# Define a transition that fades out the old source, adds a single, and then
# plays the new source.
def to_live(jingle, old, new) =
  # Fade out old source
  old = fade.final(old)

  # Superpose the jingle
  s = add([jingle, old])

  # Compose this in sequence with the new source
  sequence([s, new])
end

# A transition when switching back to files:
def to_file(old, new) =
  # We skip the file currently in new in order to being with a fresh file
  source.skip(new)
  sequence([old, new])
end

# END
# BEGIN2
# The transition to live1
jingle1 =
  single(
    "say:And now, we present the awesome show number one!!"
  )
to_live1 = to_live(jingle1)

# Transition to live2
jingle2 =
  single(
    "say:Welcome guys, this is show two on My Awesome Radio!"
  )
to_live2 = to_live(jingle2)

# Combine lives and files:
s =
  fallback(
    track_sensitive=false,
    transitions=[to_live1, to_live2, to_file],
    [live1, live2, s]
  )


# END2