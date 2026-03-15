# In #1159, we realized that the decoder
# was retuning remaining = 0 too early,
# triggering an early `on_end` call.
settings.decoder.decoders.set(["flac"])
s = single("../media/@flac[stereo].flac")

def f(rem, _) =
  if rem > 0. then test.pass() else test.fail() end
end

s.on_position(synchronous=true, remaining=true, position=0.1, f)
clock.assign_new(sync="none", [s])
output.dummy(s)
