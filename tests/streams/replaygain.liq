#!../../liquidsoap ../test.liq

def test_file_replaygain() =
  g = file.replaygain("replaygain_track_gain.mp3")
  test.not_equals(g, null())
  test.almost_equals(null.get(g), -32.)

  g = file.replaygain("r128_track_gain.mp3")
  test.not_equals(g, null())
  test.almost_equals(null.get(g), -16.)

  g = file.replaygain("replaygain_r128_track_gain.mp3")
  test.not_equals(g, null())
  test.almost_equals(null.get(g), -16.)

  g = file.replaygain("replaygain_track_gain.opus")
  test.not_equals(g, null())
  test.almost_equals(null.get(g), -32.)

  g = file.replaygain("r128_track_gain.opus")
  test.not_equals(g, null())
  test.almost_equals(null.get(g), -16.)

  g = file.replaygain("replaygain_r128_track_gain.opus")
  test.not_equals(g, null())
  test.almost_equals(null.get(g), -16.)

  g = file.replaygain("replaygain_track_gain.mp3", compute=true)
  test.not_equals(g, null())
  test.almost_equals(null.get(g), -32.)

  g = file.replaygain("replaygain_track_gain.mp3", compute=false)
  test.not_equals(g, null())
  test.almost_equals(null.get(g), -32.)

  g = file.replaygain("without_replaygain_track_gain.mp3", compute=true)
  test.not_equals(g, null())
  test.almost_equals(null.get(g), 7.39)

  g = file.replaygain("without_replaygain_track_gain.mp3", compute=false)
  test.equals(g, null())

  test.pass()
end

test.check(test_file_replaygain)
