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

# Create a fallible source. We use the experimental operator source.dynamic()
# which is bit fragile for a test.

flag = ref true
thread.run(delay=1., { flag:=false })
on = sine()
off = fail()
s = source.dynamic({ if !flag then [on] else [off] end })

test = sequence([ s, fail() ])

def check()
  if source.is_up(s) then
    test.fail()
  else
    test.pass()
  end
  shutdown()
end

output.dummy(fallback([test,on_track(fun(_)->check(),sine())]))
