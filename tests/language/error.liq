#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"
  
success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end
  
def f() =
  t(error.catch({4},fun (_) -> 5), 4)
  t(error.catch({error.raise("fail")},fun (_) -> 5), 5)
  
  if !success then test.pass() else test.fail() end
end

test.check(f)
