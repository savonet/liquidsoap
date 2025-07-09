# Stretch (as well as soundtouch) uses the Child_support clock control
# mechanism. We want to make sure that it properly propagates end-of-tracks
# to its underlying outputs/sources.
s = once(blank(duration=10.))

def on_stop() =
  test.pass()
end

o = output.dummy(fallible=true, s)
o.on_stop(on_stop)

s = stretch(ratio=2., s)

#clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
