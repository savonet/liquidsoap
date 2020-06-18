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
  n = ref(1)
  while !n < 10 do
    n := !n * 2
  end
  t(!n, 16)

  n = ref(0)
  for i in 11 do
    n := !n + i
  end
  t(!n, 55)

  n = ref(0)
  for i in 0 .. 10 do
    n := !n + i
  end
  t(!n, 55)

  n = ref(0)
  for i in [0,1,2,3,4,5,6,7,8,9,10] do
    n := !n + i
  end
  t(!n, 55)
  
  if !success then test.pass() else test.fail() end
end

test.check(f)
