#!../../liquidsoap ../test.liq

def check_value(v,v') =
  if v != v' then
    print("Expected: #{v}, got: #{v'}")
    test.fail()
  end
end

def f() =
  # This check has race conditions issues.
  check_value(int_of_float(time()), int_of_float(time.make(time.local())))
  time.zone.set("Europe/Paris")
  check_value(time.string(time=1662367396.), "Monday, 05 September 2022 10:43:16")
  test.pass()
end

test.check(f)
