def f() =
  lufs_integrated = ref(0.)

  def process(s) =
    s = lufs(s)
    source.on_frame(s, {lufs_integrated := s.lufs_integrated()})
  end

  r = request.create("@mp3[mono].mp3")

  request.process(process=process, r)

  test.almost_equal(lufs_integrated(), -4.14942548907)
  test.pass()
end

test.check(f)
