def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    test.fail()
  end
end

def f() =
  # Test strictness and evaluation order
  l = ref(false)
  r = ref(false)
  if begin l := true; true end or begin r := true; true end then () end
  t(l() and (not r()), true)

  ignore(true == false ? 5 : 6)
  test.pass()
end

test.check(f)
