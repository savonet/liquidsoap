# In #1159, we realized that the decoder
# was retuning remaining = 0 too early,
# trigering an early `on_end` call.

settings.decoder.decoders.set(["flac"])

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
