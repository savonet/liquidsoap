# The transition returned by a crossfade is add-shaped and carries no track
# mark, so `cross` has to announce the new track itself. Check that each
# crossing fires `on_track` exactly once, with the incoming track's metadata.

def titled(t, s) =
  metadata.map(
    insert_missing=true,
    update=false,
    (fun (_) -> [("title", t)]),
    s
  )
end

s =
  sequence(
    [titled("a", sine(duration=2., 440.)), titled("b", sine(duration=2., 880.))]
  )
s = crossfade(duration=1., s)

seen = ref([])

def on_track(m) =
  seen := [...seen(), m["title"]]
end

s.on_track(synchronous=true, on_track)
clock.assign_new(sync="none", [s])

o = output.dummy(fallible=true, s)

def check() =
  print(
    "titles: #{seen()}"
  )
  if seen() == ["a", "b"] then test.pass() else test.fail() end
end

thread.run(delay=5., check)
