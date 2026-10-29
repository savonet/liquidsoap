%include "../test.liq"

# The starting track's autocue fade-in is buffered in full, whatever the ending
# track's fade-out.

def autocue_track(name, fade_out) =
  m =
    autocue.metadata(
      implementation="test",
      {cue_in = 0., cue_out = 6., fade_in = 1., fade_out = fade_out}
    )

  metadata.map(
    insert_missing=true,
    id=name,
    update=false,
    (fun (_) -> [("source", name), ...m]),
    sine(duration=6., 440.)
  )
end

s =
  sequence(
    [
      autocue_track("a", 0.),
      autocue_track("b", 0.2),
      autocue_track("c", 0.),
      sine(duration=6., 440.)
    ]
  )

transitions = ref(0)

def transition(ending, starting) =
  ref.incr(transitions)
  if
    transitions() <= 2
  then
    if abs(source.remaining(starting.source) - 1.) > 0.1 then test.fail() end
    if
      float_of_string(starting.metadata["liq_fade_in"]) != 1.
    then
      test.fail()
    end
  end
  if transitions() == 2 then test.pass() end
  cross.simple(
    fade_in=1.,
    fade_out=0.,
    initial_fade_in_metadata=starting.metadata,
    initial_fade_out_metadata=ending.metadata,
    ending.source,
    starting.source
  )
end

s = cross(duration=5., transition, s)
clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
