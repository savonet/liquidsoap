def f() =
  lufs_integrated = ref(0.)

  def process(s) =
    s = lufs(s)
    s.on_frame({lufs_integrated := s.lufs_integrated()})
    s
  end

  r = request.create("@wav[stereo].wav")

  request.process(process=process, r)

  test.almost_equal(digits=3, lufs_integrated(), -0.69)
  test.pass()
end

test.check(f)
