#!../../src/liquidsoap ../../libs/main.liq

%include "../test.liq"
  
success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end
  
def f() =
  try
    ignore(file.read("mqlskjdfdjnsi"))
    success := false
  catch e in [file.error] do () end
  
  if !success then test.pass() else test.fail() end
end

test.check(f)
