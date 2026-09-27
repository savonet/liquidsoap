radio = sine()

# BEGIN
def transition(a, b) =
  if
    a.metadata["type"] == "jingle" or b.metadata["type"] == "jingle"
  then
    # Jingles play back to back with the music.
    sequence([a.source, b.source])
  elsif
    a.db_level <= -32.
    and b.db_level <= -32.
    and abs(a.db_level - b.db_level) <= 4.
  then
    # Both ends are quiet and at a similar level: full crossfade.
    cross.simple(a.source, b.source)
  elsif
    b.db_level >= a.db_level + 4. and b.db_level <= -15.
  then
    # The new track starts louder: fade out the old one only.
    add(normalize=false, [b.source, fade.out(a.source)])
  else
    # Both ends are loud: overlapping them would mask one of them.
    sequence([a.source, b.source])
  end
end

radio = cross(duration=5., transition, radio)

# END
output.dummy(radio)
