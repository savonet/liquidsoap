success = ref(true)

file = "interactive.json"

def f() =
  x = interactive.float("x", 2.)
  interactive.save(file)
  x.set(15.)
  test.equals(x(), 15.)
  interactive.load(file)
  test.equals(x(), 2.)
  try
    _ = interactive.float("bla", 0.)
    _ = interactive.float("bla", 0.)
    log.critical(
      "Did not detect double registration."
    )
    success := false
  catch e do
    log.important(
      "Double registration detected: #{e}"
    )
  end

  try
    x = interactive.float("xxx", 3.)
    x.remove()
    x.set(2.)
    success := false
  catch e do
    log.important(
      "Inexistent variable use detected: #{e}"
    )
  end

  test.pass()
end

test.check(f)
