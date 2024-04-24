def fn() =
  test.equal(int_of_string("0xff"), 255)
  test.raises(fun () -> int_of_string("ff"))
  test.equal(int_of_string(default=1, "ff"), 1)
  test.equal(float_of_string("0xff"), 255.)
  test.raises(fun () -> float_of_string("ff"))
  test.equal(float_of_string(default=1., "ff"), 1.)
  test.equal(bool_of_string("false"), false)
  test.raises(fun () -> bool_of_string("ff"))
  test.equal(bool_of_string(default=false, "ff"), false)
  test.pass()
end

test.check(fn)
