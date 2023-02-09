#!../../liquidsoap ../test.liq

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    test.fail()
  end
end

def f() =
  def double(x) = 2. * x end

  t(double(3), 6.)
  test.pass()
end

test.check(f)
