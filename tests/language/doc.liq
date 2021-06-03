#!../..//src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

a = ()

# My function
# @param ~x The parameter.
def a.b(~x)
  0
end

ignore(a)

test.pass()
