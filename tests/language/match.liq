#!../../src/liquidsoap ../../libs/stdlib.liq

%include "test.liq"
  
success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end
  
def f() =
  x = match 2
    case x do 4
  end
  
  if !success then test.pass() else test.fail() end
end

test.check(f)
