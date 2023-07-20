def f() =
  s = metronome()
  s = bpm(s)
  output.dummy(fallible=true, s)
  thread.run.recurrent({
    if s.bpm() != 0. then begin
      test.pass()
      (-1.)
    end else
      1.
    end
  })
end

test.check(f)
