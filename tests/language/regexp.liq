def f() =
  # Test basic syntax
  ignore(r/bla/gims)
  ignore(r/gni/)
  ignore(r/bla\foo\/gni/)

  # Test equality
  test.equal(r/bla/gims, r/bla/gims)

  # Test test
  test.equal(r/^foo[\d]+bar$/.test("foo1234bar"), true)
  test.equal(r/^foo[\d]+BaR$/i.test("foo1234bar"), true)
  test.equal(r/^gni/.test("bla\ngni"), false)
  test.equal(r/bla$/.test("bla\ngni"), false)
  test.equal(r/^gni/m.test("bla\ngni"), true)
  test.equal(r/bla$/m.test("bla\ngni"), true)
  test.equal(r/./.test("\n"), false)
  test.equal(r/./s.test("\n"), true)
  test.equal(r/^\/bla/.test("/blabla"), true)
  test.equal(r/^\/bla/.test("blabla"), false)

  # Test replace
  test.equal(
    r/gni/.replace(fun (_) -> "gno", "blagniblagnibla"), "blagnoblagnibla"
  )
  test.equal(
    r/gni/g.replace(fun (_) -> "gno", "blagniblagnibla"), "blagnoblagnobla"
  )

  # Test string escape.
  x = r/^\/foo$/g
  test.equal("#{x}", "r/^\\/foo$/g")
  test.pass()
end

test.check(f)
