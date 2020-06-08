#!../src/liquidsoap ../libs/pervasives.liq

%include "test.liq"

success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

def f() =
  t(getenv("BLAXXX"),"")
  t(file.is_directory("/"), true)
  t(file.is_directory("~"), true)
  t(file.is_directory("XXX"), false)

  if !success then test.pass() else test.fail() end
end

test.check(f)
