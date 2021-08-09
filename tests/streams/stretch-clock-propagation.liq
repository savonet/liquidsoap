#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Stretch (as well as soundtouch) uses the Child_support clock control
# mechamism. We want to make sure that it properly propagates end-of-tracks
# to its underlying outputs/sources.

%include "test.liq"

s = once(blank(duration=10.))

def on_stop() =
  test.pass()
end

output.dummy(fallible=true, on_stop=on_stop, s)

s = stretch(ratio=2., s)

clock.assign_new(sync="none",[s])

output.dummy(fallible=true, s)
