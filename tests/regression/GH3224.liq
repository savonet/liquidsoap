def f() =
  test.equal(r/\\/.replace(fun (_) -> "?", "foo\\bar"), "foo?bar")
  test.pass()
end

test.check(f)
