#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

def f() =
  if maybe.nothing() | "bla" != "bla" then
    test.fail()
  end

  if maybe.something("foo") | "bla" != "foo" then
    test.fail()
  end

  test.pass()
end

test.check(f)
