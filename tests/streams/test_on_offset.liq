def f() =
  s = single("./file1.mp3")
  s.on_position(
    position=1.2,
    synchronous=true,
    fun (pos, _) ->
      begin
        if s.elapsed() < 1.2 then test.fail() end
        if pos != s.elapsed() then test.fail() end
        test.pass()
      end
  )

  clock.assign_new(sync='none', [s])

  output.dummy(s)
end

test.check(f)
