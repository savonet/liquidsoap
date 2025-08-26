def f() =
  s = single("./file1.mp3")
  s.on_position(
    remaining=true,
    position=1.2,
    synchronous=true,
    fun (pos, _) ->
      begin
        test.almost_equal(pos, s.duration() - s.elapsed())
        test.pass()
      end
  )

  clock.assign_new(sync='none', [s])

  output.dummy(s)
end

test.check(f)
