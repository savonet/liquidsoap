#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

def t(x,y) =
  if x != y then test.fail() end
end

def f() =
  t(metadata.cover.remove([("a","A"),("apic","XXX"),("b","B")]), [("a","A"),("b","B")])

  test.pass()
end

test.check(f)
