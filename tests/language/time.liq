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

  def p() =
    not (10s-15s or 20s-25s or 30s-35s or 40s-45s or 50s-55s)
  end
  time.at(init=false, p, test.pass)
end

test.check(f)
