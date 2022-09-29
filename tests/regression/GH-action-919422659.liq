# In https://github.com/savonet/liquidsoap/runs/919422659,
# GH1159.liq started failing. Further investigation revealed
# that GH1159.liq was originally intended to use with the FLAC
# decoder so it was set back to only use it. This test is a
# copy of it with decoder forced to FFMPEG, the one originally
# failing in the github action run & serves as regresstion test
# for this case.

settings.decoder.decoders.set(["ffmpeg"])

s = single("../media/@flac[stereo].flac")

def f(rem, _) =
  if rem > 0. then
    test.pass()
  else
    test.fail()
  end

  shutdown()
end

s = source.on_end(s,delay=0.1,f)

clock.assign_new(sync="none",[s])

output.dummy(s)
