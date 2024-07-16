def f() =
  skip_occured = ref(false)

  def next() =
    if skip_occured() then test.pass() end
    request.create("../media/@wav[stereo].wav")
  end

  s = request.dynamic(next)
  output.dummy(fallible=true, s)

  thread.run(
    delay=1.,
    fun () ->
      begin
        skip_occured := true
        s.skip()
      end
  )
end

test.check(f)
