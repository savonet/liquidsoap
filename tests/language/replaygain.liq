#!../../liquidsoap ../test.liq

def test_metadata_replaygain() =
  test.equals(metadata.replaygain([("r128_track_gain", "0")]), 0.)
  test.equals(metadata.replaygain([("r128_track_gain", "256")]), 1.)
  test.equals(
    metadata.replaygain([("r128_track_gain", "32767")]), 32767. / 256.
  )
  test.equals(
    metadata.replaygain([("r128_track_gain", "-32768")]), -32768. / 256.
  )

  test.equals(metadata.replaygain([("r128_track_gain", "foo")]), null())
  test.equals(
    metadata.replaygain(
      [
        (
          "r128_track_gain",
          "0 foo"
        )
      ]
    ),
    null()
  )
  test.equals(metadata.replaygain([("r128_track_gain", "")]), null())

  test.equals(
    metadata.replaygain(
      [
        (
          "replaygain_track_gain",
          "1 dB"
        )
      ]
    ),
    1.
  )
  test.equals(metadata.replaygain([("replaygain_track_gain", "1")]), 1.)
  test.equals(
    metadata.replaygain(
      [
        (
          "replaygain_track_gain",
          "-1 dB"
        )
      ]
    ),
    -1.
  )
  test.equals(metadata.replaygain([("replaygain_track_gain", "-1")]), -1.)
  test.equals(
    metadata.replaygain(
      [
        (
          "replaygain_track_gain",
          "+0.424046 dB"
        )
      ]
    ),
    0.424046
  )
  test.equals(
    metadata.replaygain([("replaygain_track_gain", "+0.424046")]), 0.424046
  )
  test.equals(
    metadata.replaygain(
      [
        (
          "replaygain_track_gain",
          "-10.38500 dB"
        )
      ]
    ),
    -10.38500
  )
  test.equals(
    metadata.replaygain([("replaygain_track_gain", "-10.38500")]), -10.38500
  )

  test.equals(
    metadata.replaygain(
      [
        (
          "replaygain_track_gain",
          "1 dB"
        )
      ]
    ),
    1.
  )
  test.equals(
    metadata.replaygain(
      [
        (
          "replaygain_track_gain",
          "1 2 dB"
        )
      ]
    ),
    1.
  )
  test.equals(
    metadata.replaygain(
      [
        (
          "replaygain_track_gain",
          "1 db"
        )
      ]
    ),
    1.
  )
  test.equals(
    metadata.replaygain(
      [
        (
          "replaygain_track_gain",
          "1 DB"
        )
      ]
    ),
    1.
  )
  test.equals(
    metadata.replaygain(
      [
        (
          "replaygain_track_gain",
          "1 foo"
        )
      ]
    ),
    1.
  )

  test.equals(metadata.replaygain([("replaygain_track_gain", "")]), null())
  test.equals(metadata.replaygain([("replaygain_track_gain", "foo")]), null())

  test.equals(metadata.replaygain([]), null())
  test.equals(
    metadata.replaygain(
      [("r128_track_gain", "foo"), ("replaygain_track_gain", "1")]
    ),
    null()
  )
  test.equals(
    metadata.replaygain(
      [("replaygain_track_gain", "1"), ("r128_track_gain", "foo")]
    ),
    null()
  )

  test.pass()
end

test.check(test_metadata_replaygain)
