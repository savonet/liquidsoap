#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Correctly parse urls with empty arguments.
# https://github.com/savonet/liquidsoap/issues/123

%include "test.liq"

def f() =
  if url.split("abc?a=aa&b=&c=cc") == ("abc",[("a","aa"),("b",""),("c","cc")]) then
    test.pass()
  else
    test.fail()
  end
end

test.check(f)
