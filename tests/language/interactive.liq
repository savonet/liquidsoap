#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

file = "interactive.json"

def f() =
  x = interactive.float("x", 2.)
  interactive.save(file)
  x.set(15.)
  t(x(), 15.)
  interactive.load(file)
  t(x(), 2.)

  if !success then test.pass() else test.fail() end
end

test.check(f)
