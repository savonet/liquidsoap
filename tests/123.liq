#!../src/liquidsoap ../libs/pervasives.liq

# Correctly parse urls with empty arguments.
# https://github.com/savonet/liquidsoap/issues/123

%include "test.liq"

if url.split("abc?a=aa&b=&c=cc") == ("abc",[("a","aa"),("b",""),("c","cc")]) then
  test.pass()
else
  test.fail()
end
