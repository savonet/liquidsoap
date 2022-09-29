success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

epsilon = 0.000000001

def f() =
  x = 5.
  if abs(dB_of_lin(lin_of_dB(x))-x) >= epsilon then success := false end
  if abs(lin_of_dB(dB_of_lin(x))-x) >= epsilon then success := false end

  if !success then test.pass() else test.fail() end
end

test.check(f)
