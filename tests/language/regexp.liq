def t(x,y) =
  if x != y then
    print("#{x} != #{y}!")
    test.fail()
  end
end

def f() =
  # Test basic syntax
  ignore(r/bla/igsm)
  ignore(r/gni/)
  ignore(r/bla\foo\/gni/)

  # Test equality
  t(r/bla/igsm, r/bla/mgis)

  # Test test
  t(r/^foo[\d]+bar$/.test("foo1234bar"), true)
  t(r/^foo[\d]+BaR$/i.test("foo1234bar"), true)
  t(r/^gni/.test("bla\ngni"), false)
  t(r/bla$/.test("bla\ngni"), false)
  t(r/^gni/m.test("bla\ngni"), true)
  t(r/bla$/m.test("bla\ngni"), true)
  t(r/./.test("\n"), false)
  t(r/./s.test("\n"), true)

  # Test replace
  t(r/gni/.replace(fun (_) -> "gno", "blagniblagnibla"), "blagnoblagnibla")
  t(r/gni/g.replace(fun (_) -> "gno", "blagniblagnibla"), "blagnoblagnobla")


  # Test string escape.
  x = r/^\/foo$/g
  t("#{x}", "r/^\\/foo$/g")

  test.pass()
end

test.check(f)
