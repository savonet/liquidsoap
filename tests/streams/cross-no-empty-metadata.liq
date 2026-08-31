# `cross` replayed the incoming track's metadata at the start of the transition.
# That metadata is already inside the buffered data the replay writes into, so
# the replay only ever fired when there was none to replay, announcing an empty
# metadata downstream. Sources carrying no metadata must announce none.

a = sine(duration=3., 440.)
b = sine(duration=3., 880.)

s = sequence([a, b])
s = crossfade(deduplicate=false, duration=1., s)

seen = ref([])
s.on_metadata(synchronous=true, fun (m) -> seen := [...seen(), "#{m}"])
clock.assign_new(sync="none", [s])

o = output.dummy(fallible=true, s)

def on_stop() =
  if
    seen() == []
  then
    test.pass()
  else
    print(
      "expected no metadata, got #{seen()}"
    )
    test.fail()
  end
end

o.on_stop(synchronous=true, on_stop)
