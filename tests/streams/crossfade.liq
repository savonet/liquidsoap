#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Basic radio test

%include "test.liq"

a = sine(duration=5.,440.)
b = sine(880.)
s = sequence([a,b])
s = crossfade(duration=3., s)

output.dummy(fallible=true, s)

def on_done () =
  test.pass()
  shutdown()
end

thread.run(delay=8., on_done)
