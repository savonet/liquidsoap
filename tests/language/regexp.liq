def f() =
  # Test basic syntax
  ignore(r/bla/gims)
  ignore(r/gni/)
  ignore(r/bla\foo\/gni/)

  # Test equality
  test.equals(r/bla/gims, r/bla/gims)

  # Test test
  test.equals(r/^foo[\d]+bar$/.test("foo1234bar"), true)
  test.equals(r/^foo[\d]+BaR$/i.test("foo1234bar"), true)
  test.equals(r/^gni/.test("bla\ngni"), false)
  test.equals(r/bla$/.test("bla\ngni"), false)
  test.equals(r/^gni/m.test("bla\ngni"), true)
  test.equals(r/bla$/m.test("bla\ngni"), true)
  test.equals(r/./.test("\n"), false)
  test.equals(r/./s.test("\n"), true)
  test.equals(r/^\/bla/.test("/blabla"), true)
  test.equals(r/^\/bla/.test("blabla"), false)

  # Test replace
  test.equals(r/gni/.replace(fun (_) -> "gno", "blagniblagnibla"), "blagnoblagnibla")
  test.equals(r/gni/g.replace(fun (_) -> "gno", "blagniblagnibla"), "blagnoblagnobla")

  # Test string escape.
  x = r/^\/foo$/g
  test.equals("#{x}", "r/^\\/foo$/g")
  test.pass()
end

test.check(f)
