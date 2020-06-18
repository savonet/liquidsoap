#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

def t(x,y) =
  if x != y then test.fail() end
end

def f() =
  t(null() ?? "bla", "bla")
  t(null("foo") ?? "bla", "foo")
  t(null.case(null(),{true},fun (_) -> false),true)
  t(null.case("x",{true}, fun(_) -> false),false)

  test.pass()
end

test.check(f)
