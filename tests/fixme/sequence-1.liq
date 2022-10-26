#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Testing that head_ready is used in sequence#is_ready.
#
# We setup a sequence to start playing a source s, then stop playing the
# sequence, make s unavailable and start playing the sequence again.
# At this point the sequence should end its track and cleanup s *before*
# becoming unavailable.
#
# Pitfalls when cooking up this test:
#  - Does not work with a track insensitive switch to "instantly kill" the
#    source, because the switch makes sure to stay ready until the end of
#    track.
#  - Also does not work simply with a sequence underneath a fallible output,
#    because such an output will stop as soon as its source is not ready.

%include "test.liq"

# Create a fallible source. The simplest way is to use once() -- which is built
# from sequence(), but that is irrelevant.
s = once(sine(duration=1.))

# The imported sequence here is seq2. We put it in a fallback which will first
# play seq2, then switch to seq1 which will consume s and fail. The proper
# behavior at this point is to go back to seq2, which will notice that s has
# become unavailable and end its track and cleanup s. (By then s will be free,
# because seq1 will have also move away from it.)
test = fallback(track_sensitive=false,
                [ delay(initial=true,0.5,
                        sequence(id="seq1",[s,fail()])),
                  sequence(id="seq2",[s,fail()]) ])

def check()
  if source.is_up(s) then
    test.fail()
  else
    test.pass()
  end
  shutdown()
end

output.dummy(fallback([test,on_track(fun(_)->check(),sine())]))
