#!../src/liquidsoap ../libs/pervasives.liq

%include "test.liq"

# Test getters, see #1181

def f(~x)
  x = to_int_getter(x)
  ignore(x() + 2)
end

f(x=3)
f(x={4})

def g(~x=int_getter(2))
  x = to_int_getter(x)
  ignore(x() + 2)
end

g()
g(x=3)
g(x={4})

test.pass()
