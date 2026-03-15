def f() =
  s = single("./file1.mp3")
  s.on_position(
    remaining=true,
    position=1.2,
    allow_partial=true,
    synchronous=true,
    fun (pos, _) ->
      begin
        if s.duration() - s.elapsed() < 1.2 then test.fail() end
        test.almost_equal(pos, s.duration() - s.elapsed())
        test.pass()
      end
  )
  s.on_track(synchronous=true, fun (_) -> s.skip())

  clock.assign_new(sync='none', [s])

  output.dummy(s)
end

test.check(f)
