let {audio = a} = source.tracks(sine())

track_l = track.audio.amplify(2., track.audio.stereo.pan(-1., a))
track_r = track.audio.amplify(0.5, track.audio.stereo.pan(1., a))

s = source({audio=track.audio.add(normalize=false, [track_l, track_r])})

r = rms.stereo(s, duration=1.)

rl = rms(stereo.left(s), duration=1.)
rr = rms(stereo.right(s), duration=1.)

output.dummy(r)

output.dummy(rl)
output.dummy(rr)

def check_peak() =
  let (r_l, r_r) = r.rms()
  rl = rl.rms()
  rr = rr.rms()
  test.almost_equal(r_l, rl)
  test.almost_equal(r_r, rr)
  test.pass()
end

thread.run(check_peak, delay=1.)

output.dummy(s)
