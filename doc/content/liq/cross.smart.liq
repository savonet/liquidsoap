music = sine()
jingles = sine()

# BEGIN
# A function to add a source_tag metadata to a source:
def source_tag(s, tag) =
  def f(_) =
    [("source_tag", (tag : string))]
  end
  metadata.map(id=tag, insert_missing=true, f, s)
end

# Tag our sources
music = source_tag(music, "music")
jingles = source_tag(jingles, "jingles")

# Combine them with one jingle every 3 music tracks
radio = rotate(weights=[1, 3], [jingles, music])

# Now a custom crossfade transition:
def transition(a, b) =
  # If old or new source is not music, no fade
  if
    a.metadata["source_tag"] != "music" or a.metadata["source_tag"] != "music"
  then
    sequence([a.source, b.source])
  else
    # Else, apply the standard transition
    cross.simple(a, b)
  end
end

# Apply it!
radio = cross(duration=5., transition, radio)

# END
output.dummy(fallible=true, radio)
