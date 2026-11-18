%include "../test.liq"

# For one track start, the callbacks of the source that produces the track
# run before those of the operators that relay it, innermost first, and each
# level is called once.

def tracks() =
  source.dynamic(track_sensitive=true, {sine(duration=0.07)})
end

inner = tracks()
middle = fallback([inner])
outer = fallback([middle])

calls = ref([])
inner.on_track(synchronous=true, fun (_) -> calls := [...calls(), "inner"])
middle.on_track(synchronous=true, fun (_) -> calls := [...calls(), "middle"])

rounds = 20

def on_track(_) =
  calls := [...calls(), "outer"]
  if
    list.length(calls()) == 3 * rounds
  then
    expected =
      list.flatten(list.init(rounds, fun (_) -> ["inner", "middle", "outer"]))

    test.equal(calls(), expected)
    test.pass()
  end
end

outer.on_track(synchronous=true, on_track)
clock.assign_new(sync="none", [outer])
output.dummy(fallible=true, outer)
