a = sine(duration=5., amplitude=0.01, 440.)
b = sine(amplitude=0.1, 880.)
s = sequence([a,b])
s = crossfade(smart=true, duration=3., s)

output.dummy(fallible=true, s)

def on_done () =
  test.pass()
  shutdown()
end

thread.run(delay=8., on_done)
