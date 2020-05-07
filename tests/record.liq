#!../src/liquidsoap ../libs/pervasives.liq

success = ref(true)

%include "test.liq"

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

r = {{ 2 | a = 8 , b = 12 , f = fun(x) -> 2 * x }}
t(1+r, 3)
t(r.b, 12)
t(r.a, 8)
t(r.f(5), 10)

if !success then test.pass() else test.fail() end
