#!../../src/liquidsoap ../../libs/pervasives.liq

success = ref(true)

%include "test.liq"

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

def f() =
  # Basic checks
  n = 2
  r = n.{ a = 8 , b = 12 , f = fun(x) -> 2 * x }
  t(1+r, 3)
  t(r.b, 12)
  t(r.a, 8)
  t(r.f(5), 10)
  #TODO: the following fails because it gets parsed as the float 2. followed by the record {a = 5}...
  #r = 2.{a = 5}

  # Test overriding with subfields
  r = ()
  let r.f = ()
  let r.f.variant = 7
  let r.f = r.f.variant
  ignore(r)

  # Test replacing subfields
  r = ()
  let r.a = ()
  let r.a.b = 5
  let replaces r.a = 2
  ignore(r.a.b)
  def replaces r.a (x) = 2*x end
  ignore(r.a.b)

  # A resetable counter
  n = ref(0)
  def counter()
    n := !n + 1
    !n
  end
  def counter.reset()
    n := 0
  end

  ignore(counter())
  ignore(counter())
  t(counter(), 3)
  counter.reset()
  t(counter(), 1)

  if !success then test.pass() else test.fail() end
end

test.check(f)