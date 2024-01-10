def f() =
  test.equals(environment.get("BLAXXX"), "")
  test.equals(file.is_directory("/"), true)
  test.equals(file.is_directory("~"), true)
  test.equals(file.is_directory("XXX"), false)
  test.equals(liquidsoap.version.at_least("2.0.0"), true)
  test.equals(liquidsoap.version.at_least("666.0.0"), false)
  test.pass()
end

test.check(f)
