#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

video.frame.rate.set(24)

a = sine(duration=5., 440.)
b = sine(880.)
s = sequence([a,b])
s = crossfade(duration=3., s)

output.dummy(fallible=true, s)

def on_done () =
  test.pass()
  shutdown()
end

thread.run(delay=8., on_done)
