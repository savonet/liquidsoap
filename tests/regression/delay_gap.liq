# Regression test: delay(d, s) must keep s unavailable for d seconds between
# tracks, i.e. the gap must actually be applied. We chop a sine into short
# tracks and require consecutive track boundaries to be at least half the delay
# apart (they would only be ~every apart without the delay).

s = sine()
s = chop(every=0.2, s)
s = delay(1.0, s)

last = ref(null)
s.on_track(
  synchronous=true,
  fun (_) ->
    begin
      t = time()
      prev = last()
      last := t
      if null.defined(prev) and t - null.get(prev) >= 0.5 then test.pass() end
    end
)

output.dummy(fallible=true, s)

thread.run(delay=15., {test.fail()})
