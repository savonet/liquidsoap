# `cross` no longer replays the outgoing track's metadata into the transition, so
# the transition's argument is the only way that metadata reaches user code.
# `crossfade` reads it there for `initial_fade_out_metadata`, so an override on
# the outgoing track is silently ignored if the argument stops carrying it.
# Check that each crossing hands the transition the right track on either side,
# and that an override set on the outgoing track survives the trip.

def titled(t, extra, s) =
  metadata.map(
    insert_missing=true,
    update=false,
    (fun (_) -> [("title", t), ...extra]),
    s
  )
end

s =
  sequence(
    [
      titled("a", [("liq_fade_out", "0.5")], sine(duration=3., 440.)),
      titled("b", [], sine(duration=3., 880.)),
      titled("c", [], sine(duration=3., 660.))
    ]
  )

seen = ref([])

def transition(before, after) =
  seen :=
    [
      ...seen(),
      (
        before.metadata["title"],
        after.metadata["title"],
        before.metadata["liq_fade_out"] != ""
      )
    ]

  add(normalize=false, [before.source, after.source])
end

s = cross(duration=1., transition, s)
clock.assign_new(sync="none", [s])

o = output.dummy(fallible=true, s)

def on_stop() =
  if
    seen() == [("a", "b", true), ("b", "c", false)]
  then
    test.pass()
  else
    print(
      "expected [(\"a\", \"b\", true), (\"b\", \"c\", false)], got #{seen()}"
    )
    test.fail()
  end
end

o.on_stop(synchronous=true, on_stop)
