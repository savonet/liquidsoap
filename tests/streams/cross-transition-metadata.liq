# `cross` used to replay the outgoing track's metadata into the transition, so a
# transition keeping the outgoing source announced that track a second time. With
# an `add`-shaped transition both replays landed at the same frame position and
# only one of them survived, which dropped the incoming track's own
# announcement. Check that each track is announced exactly once, in order.

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
      titled("a", sine(duration=3., 440.)),
      titled("b", sine(duration=3., 880.)),
      titled("c", sine(duration=3., 660.))
    ]
  )

def transition(a, b) =
  add(normalize=false, [a.source, b.source])
end

s = cross(deduplicate=false, duration=1., transition, s)

seen = ref([])
s.on_metadata(synchronous=true, fun (m) -> seen := [...seen(), m["title"]])
clock.assign_new(sync="none", [s])

o = output.dummy(fallible=true, s)

def on_stop() =
  if
    seen() == ["a", "b", "c"]
  then
    test.pass()
  else
    print(
      "expected [\"a\", \"b\", \"c\"], got #{seen()}"
    )
    test.fail()
  end
end

o.on_stop(synchronous=true, on_stop)
