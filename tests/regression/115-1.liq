#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

s = add(normalize=true,[
  sine(),
  sequence([sine(amplitude=0.3,duration=1.),
            once(sine(duration=1.))])
])

output.dummy(s)
thread.run(delay=3., {test.pass(); shutdown()})
