#!../../liquidsoap ../test.liq

def test_file_replaygain() =
  g = file.replaygain("file1.mp3")
  test.not_equals(g, null())
  test.almost_equals(null.get(g), 7.39)

  test.pass()
end

test.check(test_file_replaygain)
