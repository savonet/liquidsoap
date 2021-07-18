#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

video.frame.rate.set(24)

a = sine(duration=5.,440.)
b = sine(880.)
s = sequence([a,b])

def t(a, b)
  log.important("Crossing!")
  log.important("Levels: #{a.db_level} / #{b.db_level}")
  log.important("Metadata: #{a.metadata} / #{b.metadata}")
  add(normalize=false, [a.source, b.source])
end

s = cross(duration=3., width=1., t, s)

output.dummy(fallible=true, s)

def on_done () =
  test.pass()
  shutdown()
end

thread.run(delay=8., on_done)
