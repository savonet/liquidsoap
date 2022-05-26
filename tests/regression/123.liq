# Correctly parse urls with empty arguments.
# https://github.com/savonet/liquidsoap/issues/123

def f() =
  if url.split("abc?a=aa&b=&c=cc") == ("abc",[("a","aa"),("b",""),("c","cc")]) then
    test.pass()
  else
    test.fail()
  end
end

test.check(f)
