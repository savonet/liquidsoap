success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

def f() =
  t(environment.get("BLAXXX"),"")
  t(file.is_directory("/"), true)
  t(file.is_directory("~"), true)
  t(file.is_directory("XXX"), false)

  t(liquidsoap.version.at_least("2.0.0"), true)
  t(liquidsoap.version.at_least("666.0.0"), false)

  if success() then test.pass() else test.fail() end
end

test.check(f)
