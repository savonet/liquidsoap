# Regression test for https://github.com/savonet/liquidsoap/issues/5398
# Last.fm rejects Audioscrobbler requests sent to the former HTTP default.

contents = file.contents("../../src/libs/extra/audioscrobbler.liq")

if
  string.contains(
    substring='~base_url="https://ws.audioscrobbler.com/2.0"',
    contents
  ) and not string.contains(
    substring='~base_url="http://ws.audioscrobbler.com/2.0"',
    contents
  )
then
  test.pass()
else
  test.fail("Audioscrobbler must default to the Last.fm HTTPS endpoint")
end
