%include "test.liq"

# In #1159, we realized that the decoder
# was retuning remaining = 0 too early,
# trigering an early `on_end` call.

s = single("../media/files/audio/@flac(stereo).flac")

def f(rem, _) =
  if rem > 0. then
    test.pass()
  else
    test.fail()
  end

  shutdown()
end

s = on_end(delay=0.1,f,s)

clock.assign_new(sync="none",[s])

output.dummy(s)
