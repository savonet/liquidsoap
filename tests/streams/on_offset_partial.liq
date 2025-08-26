def f() =
  s = single("./file1.mp3")
  s.on_position(
    position=9999999999.,
    allow_partial=true,
    synchronous=true,
    fun (pos, _) ->
      begin
        if pos != s.elapsed() then test.fail() end
        test.pass()
      end
  )

  clock.assign_new(sync='none', [s])

  output.dummy(s)
end

test.check(f)
