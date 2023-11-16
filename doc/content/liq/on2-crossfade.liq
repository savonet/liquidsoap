# Our custom crossfade that only crossfade between tracks.
def my_crossfade(s) =
  # Our transition function
  def f(_,_, old_m, new_m, old, new) =
    # If none of old and new have "type" metadata with value "jingles", we
    # crossfade the source:
    if old_m["type"] != "jingle" and new_m["type"] != "jingle" then
      add([fade.initial(new), fade.final(old)])
    else
      sequence([old,new])
    end
 end
 # Now, we apply smart_cross with this function:
 smart_cross(f,s)
end
