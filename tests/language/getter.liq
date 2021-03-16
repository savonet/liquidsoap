#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

# Test getters, see #1181

def t(x,y) =
  if x != y then test.fail() end
end

def f(~x)
  ignore(getter.get(x) + 2)
end

def g(~x=getter(2))
  x = getter.function(x)
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

  t(getter.get(getter(3)), 3)
  t(getter.get(getter({3})), 3)
  t(getter.get(getter.map(fun (x) -> 2*x, getter(3))), 6)
  t(getter.get(getter.map(fun (x) -> 2*x, getter({3}))), 6)
  t(getter.is_constant(getter(3)), true)
  t(getter.is_constant(getter({3})), false)

  test.pass()
end

test.check(tests)
