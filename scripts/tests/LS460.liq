# Scenario:
#  Let foo start using q, then stop it, skip in q.
#  When foo restarts it doesn't know that q isn't ready anymore,
#  which can lead to a crash.

q = once(sine(duration=10.))
output.dummy(id="bar",mksafe(q))
output.dummy(id="foo",fallback([amplify(1.,q),blank(duration=1.)]))

def at(t,s)
  add_timeout(t,{ignore(server.execute(s));(-1.)})
end

at(3.,"foo.stop")
at(4.,"bar.skip")
at(5.,"foo.start")
add_timeout(6.,{print("TEST PASSED");shutdown();(-1.)})
