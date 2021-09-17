#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

log.level.set(4)

def f(_)
  print("Detected sine.")
  test.pass()
  shutdown()
end

s = sine(440.)
s = sine.detect(debug=false, [440.], s, f)
output.dummy(s)
