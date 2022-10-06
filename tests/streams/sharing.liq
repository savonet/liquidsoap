#!../../liquidsoap ../test.liq

# Test whether modifying one source does not affect a copy of it. See #2170.

s = sine()
s = add(normalize=false, [s, amplify(0., s)])
s = rms(s)
rms = s.rms
output.dummy(s)

def on_done () =
  print("RMS: #{rms()}")
  if rms() > 0.5 then test.pass() else test.fail() end
  shutdown()
end

thread.run(delay=2., on_done)
