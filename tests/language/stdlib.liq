def f() =
  test.equal(environment.get("BLAXXX"), "")
  test.equal(file.is_directory("/"), true)
  test.equal(file.is_directory("~"), true)
  test.equal(file.is_directory("XXX"), false)
  test.equal(liquidsoap.version.at_least("2.0.0"), true)
  test.equal(liquidsoap.version.at_least("666.0.0"), false)
  test.pass()
end

test.check(f)
