def f() =
  test.equal(false ? 123 : 456  == 123, false)
  test.pass()
end

test.check(f)
