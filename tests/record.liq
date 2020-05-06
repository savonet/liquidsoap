#!../src/liquidsoap ../libs/pervasives.liq

success = ref(true)

def test(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

%include "test.liq"

r = {{ 2 | a = 8 , b = 12 , f = fun(x) -> 2 * x }}
test(r+1, 3)
test(r.b, 12)
test(r.a, 8)
test(r.f(5), 10)

if !success then test.pass() else test.fail() end
