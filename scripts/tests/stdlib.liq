#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

success = ref(true)

def test(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

test(getenv("BLAXXX"),"")

if !success then test.pass() else test.fail() end
