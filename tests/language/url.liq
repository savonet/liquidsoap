#!../../liquidsoap ../test.liq

def f() =
  # Correctly parse urls with empty arguments.
  # https://github.com/savonet/liquidsoap/issues/123
  test.equals(url.split("abc?a=aa&b=&c=cc"), ("abc",[("a","aa"),("b",""),("c","cc")]))
  # No argument (#3126).
  test.equals(url.split("/bla"), ("/bla", []))
  test.pass()
end

test.check(f)
