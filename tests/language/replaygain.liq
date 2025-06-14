#!../../liquidsoap ../test.liq

def test_metadata_replaygain() =
  test.equal(metadata.replaygain([("r128_track_gain", "0")]), 0.)
  test.equal(metadata.replaygain([("r128_track_gain", "256")]), 1.)
  test.equal(metadata.replaygain([("r128_track_gain", "32767")]), 32767. / 256.)
  test.equal(
    metadata.replaygain([("r128_track_gain", "-32768")]), -32768. / 256.
  )

  test.equal(metadata.replaygain([("r128_track_gain", "foo")]), null)
  test.equal(
    metadata.replaygain(
      [
        (
          "r128_track_gain",
          "0 foo"
        )
      ]
    ),
    null
  )
  test.equal(metadata.replaygain([("r128_track_gain", "")]), null)

  test.equal(
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
  test.equal(metadata.replaygain([("replaygain_track_gain", "1")]), 1.)
  test.equal(
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
  test.equal(metadata.replaygain([("replaygain_track_gain", "-1")]), -1.)
  test.equal(
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
  test.equal(
    metadata.replaygain([("replaygain_track_gain", "+0.424046")]), 0.424046
  )
  test.equal(
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
  test.equal(
    metadata.replaygain([("replaygain_track_gain", "-10.38500")]), -10.38500
  )

  test.equal(
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
  test.equal(
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
  test.equal(
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
  test.equal(
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
  test.equal(
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

  test.equal(metadata.replaygain([("replaygain_track_gain", "")]), null)
  test.equal(metadata.replaygain([("replaygain_track_gain", "foo")]), null)

  test.equal(metadata.replaygain([]), null)
  test.equal(
    metadata.replaygain(
      [("r128_track_gain", "foo"), ("replaygain_track_gain", "1")]
    ),
    null
  )
  test.equal(
    metadata.replaygain(
      [("replaygain_track_gain", "1"), ("r128_track_gain", "foo")]
    ),
    null
  )

  test.pass()
end

test.check(test_metadata_replaygain)
