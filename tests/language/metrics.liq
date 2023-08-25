#!../../liquidsoap ../test.liq

def f() =
  test.metric(category="language", name="functions", value=float(liquidsoap.functions.count()), unit="functions")
  test.pass()
end

test.check(f)
