# Our custom crossfade that only crossfade between tracks.
def my_crossfade(s) =
  # Our transition function
  def f(a, b) =
    # If none of old and new have "type" metadata with value "jingles", we
    # crossfade the source:
    if
      a.metadata["type"] != "jingle" and b.metadata["type"] != "jingle"
    then
      add([fade.in(a.source), fade.out(b.source)])
    else
      sequence([a.source, b.source])
    end
  end

  # Now, we apply smart_cross with this function:
  cross(f, s)
end
