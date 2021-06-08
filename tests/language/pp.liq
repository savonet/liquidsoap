#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end
  
def f() =
  %ifdef sine
    x = 0
  %endif
  t(x, 0)

  %ifdef sine
    x = 1
  %else
    x = 2
  %endif
  t(x, 1)

  %ifdef xxx
    x = 3
  %else
    x = 4
  %endif
  t(x, 4)

  if !success then test.pass() else test.fail() end
end

test.check(f)
