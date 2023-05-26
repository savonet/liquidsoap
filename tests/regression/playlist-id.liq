def t(file, expected) =
  id = playlist.id(default="foo", file)
  if id != expected then
    print("Test fail: #{file} maps to: #{id} instead of #{expected}")
    test.fail()
  end
end

def f() =
  t("bla", "bla")
  t("bla/gni.pls", "gni.pls")
  t("./gni.pls", "gni.pls")
  t(".", "foo")

  test.pass()
end

test.check(f)
