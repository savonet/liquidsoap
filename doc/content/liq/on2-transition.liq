live1 = sine()
live2 = sine()
s = sine()

# TODO: we have to cast to source below, which is bad...

# BEGIN
# Define a transition that fades out the old source, adds a single, and then
# plays the new source.
def to_live(jingle, old, new) =
  # Fade out old source
  old = fade.out((old:source))

  # Superpose the jingle
  s = add([(jingle:source), old])

  # Compose this in sequence with the new source
  sequence([s, new])
end

# A transition when switching back to files.
def to_file(old, new) =
  # We skip the file currently in new in order to being with a fresh file.
  source.skip((new:source))
  sequence([(old:source), new])
end

# END
# BEGIN2
# The transition to live1
jingle1 =
  single(
    "say:And now, we present the awesome show number one!!"
  )
to_live1 = fun(old, new) -> to_live(jingle1, old, new)

# Transition to live2
jingle2 =
  single(
    "say:Welcome guys, this is show two on My Awesome Radio!"
  )
to_live2 = fun(old, new) -> to_live(jingle2, old, new)

# Combine lives and files:
s =
  fallback(
    track_sensitive=false,
    transitions=[to_live1, to_live2, to_file],
    [live1, live2, s]
  )
# END2
output.dummy(fallible=true, s)
