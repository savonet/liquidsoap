#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

# Test getters, see #1181

def f(~x)
  x = to_int_getter(x)
  ignore(x() + 2)
end

def g(~x=int_getter(2))
  x = to_int_getter(x)
  ignore(x() + 2)
end

def tests() =
  f(x=3)
  f(x={4})

  g()
  g(x=3)
  g(x={4})

  r = ref(5)
  r = ref.getter(r)
  f(x=r)
  g(x=r)
  r.set(r()+2)

  test.pass()
end

test.check(tests)
