def f() =
  s = metronome()
  s = bpm(s)
  clock.assign_new(sync="none",[s])
  output.dummy(fallible=true, s)
  thread.run(delay=0.1, {if s.bpm() != 0. then test.pass() else test.fail () end})
end

test.check(f)
