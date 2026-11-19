%include "../test.liq"

# When a switch hands off between a source that paces itself and one that
# does not, the clock follows at its next tick: by the first frame after the
# one in which the handoff happened.

paced = source.dynamic(id="paced", self_sync=true, {sine(440.)})
free = sine(id="free", 880.)

use_paced = ref(false)
s = switch([(use_paced, paced), ({true}, free)])

frames = ref(0)
was_paced = ref(false)
handoffs = ref(0)

def on_frame() =
  ref.incr(frames)
  if frames() == 20 then use_paced := true end
  if frames() == 60 then use_paced := false end

  selected = s.selected()
  is_paced = null.defined(selected) and source.id(null.get(selected)) == "paced"

  # The switch read another source during the previous frame.
  if
    is_paced != was_paced()
  then
    was_paced := is_paced
    ref.incr(handoffs)
    if
      clock(s.clock).self_sync() != is_paced
    then
      test.fail(
        "frame #{frames()}: switch on paced source: #{is_paced}, clock following \
         a sync source: #{clock(s.clock).self_sync()}"
      )
    end
  end

  if
    frames() == 100
  then
    if
      handoffs() == 2
    then
      test.pass()
    else
      test.fail(
        "#{handoffs()} handoffs"
      )
    end
  end
end

s.on_frame(synchronous=true, on_frame)
output.dummy(fallible=true, s)
