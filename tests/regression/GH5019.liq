s =
  sequence(
    [
      sine(duration=15., 440.),
      blank(duration=2.),
      sine(duration=15., 440.),
      blank(duration=2.),
      sine(duration=15., 440.)
    ]
  )

s =
  fade.in(
    duration=5.,
    track_sensitive=true,
    fade.out(duration=5., track_sensitive=true, s)
  )

o =
  output.file(
    %wav(stereo = true, channels = 2, samplesize = 16, header = true),
    fallible=true,
    "GH5019.wav",
    s
  )

clock.assign_new(sync="none", [s])

o.on_stop(synchronous=true, test.pass)
