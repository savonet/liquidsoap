# Basic radio test

mic = sine()
bed = mksafe(playlist("playlist"))

radio = add([mic, bed])

output.dummy(fallible=true, radio)

def on_done () =
  test.pass()
end

thread.run(delay=10., on_done)
