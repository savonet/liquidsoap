#!../../liquidsoap ../test.liq

def f() =
  if int_of_float(time()) != int_of_float(time.make(time.local())) then test.fail() end
  if time.string(time=1662367396.) != "Monday, 05 September 2022 10:43:16" then test.fail() end
  test.pass()
end

test.check(f) 
