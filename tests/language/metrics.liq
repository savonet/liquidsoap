#!../../liquidsoap ../test.liq
def f() =
  n = liquidsoap.functions.count()
  print(
    "Functions: #{n}."
  )
  test.metric(
    category="language",
    name="functions",
    value=float(n),
    unit="functions"
  )

  n = protocol.count()
  print(
    "Protocols: #{n}."
  )
  test.metric(
    category="language",
    name="protocols",
    value=float(n),
    unit="protocols"
  )

  test.pass()
end

test.check(f)
