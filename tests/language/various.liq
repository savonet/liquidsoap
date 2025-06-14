def f() =
  test.equal(false ? 123 : 456  == 123, false)
  test.equal(path.basename(liquidsoap.script.path), "various.liq")
  test.pass()
end

test.check(f)
