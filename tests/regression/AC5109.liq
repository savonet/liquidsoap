# See: https://github.com/AzuraCast/AzuraCast/issues/5109.
# Crossfade transition sources with breaks where triggering
# end of transition too early.

s1 = sine(duration=20.)
s2 = sine(duration=20.)
s = sequence([s1,s2])

def transition(a, b) =
  s1 = sine(duration=0.5)
  s1 = metadata.map(fun (_) -> [("type", "s1")], s1)

  s2 = sine(duration=0.5)
  s2 = metadata.map(fun (_) -> [("type", "s2")], s2)
  sequence([(a.source:source), (s1:source), (s2:source), (b.source:source)])
end

s = cross(transition, s)

def check(m) =
  if m["type"] == "s2" then
    test.pass()
  end
end

s.on_metadata(check)

clock.assign_new(sync="none",[s])

output.dummy(fallible=true,s)
