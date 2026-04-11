def f() =
  lufs_integrated = ref(0.)
  true_peak = ref(neg_infinity)

  def process(s) =
    s = lufs(s)
    def update() =
      lufs_integrated := s.lufs_integrated()
      true_peak := s.true_peak()
    end
    s.on_frame(synchronous=true, update)
    s
  end

  r = request.create("@wav[stereo].wav")
  request.process(process=process, r)

  # Integrated LUFS should match the known value for the test fixture.
  test.almost_equal(digits=3, lufs_integrated(), -0.69)

  # True peak must be a finite value (not neg_infinity) after processing.
  test.check(true_peak() > neg_infinity)

  # True peak of the test fixture must be at or below 0 dBTP (no clipping).
  test.check(true_peak() <= 0.)

  test.pass()
end

test.check(f)
