#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

def t(x,y) =
  if x != y then test.fail() end
end

def f() =
  t(null() ?? "bla", "bla")
  t(null("foo") ?? "bla", "foo")
  t(null.case(null(),{true},fun (_) -> false),true)
  t(null.case("x",{true}, fun(_) -> false),false)
  t(null.get("x"), "x")
  t(null.get(default="x", "y"), "y")
  t(null.get(default="x", null()), "x")
  t(null.find(fun (x) -> if x mod 2 == 0 then 2*x else null() end, [1,3,2,5]), 4)

  test.pass()
end

test.check(f)
