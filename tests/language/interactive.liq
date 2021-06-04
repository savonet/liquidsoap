#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

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
  try
    interactive.float("bla", 0.)
    interactive.float("bla", 0.)
    log.critical("Did not detect double registration.")
    success := false
  catch _ do
    log.important("Double registration detected.")
  end

  if !success then test.pass() else test.fail() end
end

test.check(f)
