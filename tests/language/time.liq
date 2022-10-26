#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

def f() =
  if int_of_float(time()) == int_of_float(time.make(time.local())) then
    test.pass()
  else
    test.fail()
  end
end

test.check(f)
