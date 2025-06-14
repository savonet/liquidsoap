#!../../liquidsoap ../test.liq

def test_file_replaygain() =
  g = file.replaygain("replaygain_track_gain.mp3")
  test.not.equal(g, null)
  test.almost_equal(null.get(g), -32.)

  g = file.replaygain("r128_track_gain.mp3")
  test.not.equal(g, null)
  test.almost_equal(null.get(g), -16.)

  g = file.replaygain("replaygain_r128_track_gain.mp3")
  test.not.equal(g, null)
  test.almost_equal(null.get(g), -16.)

  g = file.replaygain("replaygain_track_gain.opus")
  test.not.equal(g, null)
  test.almost_equal(null.get(g), -32.)

  g = file.replaygain("r128_track_gain.opus")
  test.not.equal(g, null)
  test.almost_equal(null.get(g), -16.)

  g = file.replaygain("replaygain_r128_track_gain.opus")
  test.not.equal(g, null)
  test.almost_equal(null.get(g), -16.)

  g = file.replaygain("replaygain_track_gain.mp3", compute=true)
  test.not.equal(g, null)
  test.almost_equal(null.get(g), -32.)

  g = file.replaygain("replaygain_track_gain.mp3", compute=false)
  test.not.equal(g, null)
  test.almost_equal(null.get(g), -32.)

  g = file.replaygain("without_replaygain_track_gain.mp3", compute=true)
  test.not.equal(g, null)
  test.almost_equal(null.get(g), 7.39)

  g = file.replaygain("without_replaygain_track_gain.mp3", compute=false)
  test.equal(g, null)

  test.pass()
end

test.check(test_file_replaygain)
