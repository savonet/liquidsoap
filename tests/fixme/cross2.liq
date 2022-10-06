#!../../liquidsoap ../test.liq

# See #2080.

s = amplify(0.5, sequence([sine(duration=5.), sine(500.)]))
jingle = playlist("files/jingles")

# The subtlety here is that once(jingle) returns a source with methods (such as
# id). The inference thus thinks that a.source has methods. And the function f
# thus cannot be passed to cross because it is not general enough.

def f(a, b)
  sequence([a.source, once(jingle), b.source])
end

s = cross(f, s)

output.dummy(s)

def on_done () =
  test.pass()
  shutdown()
end
thread.run(delay=8., on_done)
