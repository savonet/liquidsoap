# A transition is free to drop the incoming source. `cross` has already
# consumed that track's metadata into the buffer it hands the transition, so
# dropping the source dropped the announcement with it, while the rest of the
# track still played once the transition was over: audible, unannounced.
#
# `cross` announces it on the tail instead, when that audio starts, and only
# when the transition did not announce it itself. The third track repeats the
# second, so this also checks the fallback is not mistaken for a repeat.

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
    [
      titled("a", sine(duration=4., 440.)),
      titled("b", sine(duration=4., 880.)),
      titled("b", sine(duration=4., 660.))
    ]
  )

def transition(before, _) =
  before.source
end

s = cross(duration=1., transition, s)

seen = ref([])

def on_metadata(m) =
  t = m["title"]
  seen := [...seen(), t]
end

s.on_metadata(synchronous=true, on_metadata)
clock.assign_new(sync="none", [s])

o = output.dummy(fallible=true, s)

def on_stop() =
  if
    seen() == ["a", "b", "b"]
  then
    test.pass()
  else
    print(
      "expected [\"a\", \"b\", \"b\"], got #{seen()}"
    )
    test.fail()
  end
end

o.on_stop(synchronous=true, on_stop)
