flac_p = "/usr/bin/flac"

def test_flac(f) =
  if
    process.test(
      "which metaflac"
    )
  then
    channels =
      list.hd(
        default="",
        process.read.lines(
          "metaflac --show-channels #{process.quote(f)} 2>/dev/null"
        )
      )

    #If the value is not an int, this returns 0 and we are ok :)
    int_of_string(channels)
  else
    if
      string.match(pattern="flac", f)
    then
      # We do not know the number of audio channels so setting to -1
      (-1)
    else
      # All tests failed: no audio decodable using flac..
      0
    end
  end
end

decoder.add(
  name="FLAC",
  description=
    "Decode files using the flac decoder binary.",
  test=test_flac,
  flac_p
)
