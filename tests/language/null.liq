#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

def f() =
  if (null() ?? "bla") != "bla" then
    test.fail()
  end

  if (null("foo") ?? "bla") != "foo" then
    test.fail()
  end

  test.pass()
end

test.check(f)
