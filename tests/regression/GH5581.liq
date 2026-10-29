%include "../test.liq"

# The ending track's autocue fade sets the transition length, whatever the
# starting track's own cross duration.

def autocue_track(name, fade_out) =
  m =
    autocue.metadata(
      implementation="test",
      {cue_in = 0., cue_out = 6., fade_in = 0., fade_out = fade_out}
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
    [autocue_track("a", 3.), autocue_track("b", 1.), sine(duration=6., 440.)]
  )

transitions = ref(0)

def transition(ending, starting) =
  ref.incr(transitions)

  if
    transitions() == 1
  then
    if abs(source.remaining(ending.source) - 3.) > 0.1 then test.fail() end
    if abs(source.remaining(starting.source) - 3.) > 0.1 then test.fail() end
  end

  if
    transitions() == 2
  then
    if abs(source.remaining(ending.source) - 1.) > 0.1 then test.fail() end
    test.pass()
  end

  add([ending.source, starting.source])
end

s = cross(duration=5., transition, s)
clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
