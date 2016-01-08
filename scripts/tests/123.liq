# Correctly parse urls with empty arguments.
# https://github.com/savonet/liquidsoap/issues/123

if url.split("abc?a=aa&b=&c=cc") == ("abc",[("a","aa"),("b",""),("c","cc")]) then
  print("TEST PASSED")
else
  print("TEST FAILED")
end
