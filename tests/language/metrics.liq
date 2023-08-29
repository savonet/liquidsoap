#!../../liquidsoap ../test.liq

def f() =
  n = liquidsoap.functions.count()
  print("We have #{n} functions.")
  test.metric(category="language", name="functions", value=float(n), unit="functions")
  test.pass()
end

test.check(f)
