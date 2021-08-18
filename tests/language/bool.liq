#!../../src/liquidsoap ../../libs/stdlib.liq

%include "test.liq"

def f() =
  ignore(true == false ? 5 : 6)
  test.pass()
end

test.check(f)
