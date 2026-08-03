log.level.set(4)

# `on_leave` must fire on every handoff, and its `track_sensitive` argument must
# say how the source ended: `true` when it had nothing left for its current
# track, `false` when the switch cut into a track it was still playing.
#
# The boundary case used to never fire at all: no transition is built at a
# boundary, and `on_leave` was attached to the transition.

boundary_leave = ref(null)
preempt_leave = ref(null)

# Ends its tracks on its own: leaving it is always a boundary.
at_boundary =
  (blank(id="at_boundary", duration=0.5) : source).{
    on_leave = fun (x) -> boundary_leave := x.track_sensitive
  }

# Never ends: the only way to leave it is to cut in.
preempted =
  (sine(id="preempted") : source).{
    track_sensitive = false,
    on_leave = fun (x) -> preempt_leave := x.track_sensitive
  }

use_boundary = ref(false)
use_preempted = ref(true)

s =
  switch(
    [
      ({use_boundary()}, at_boundary),
      ({use_preempted()}, preempted),
      ({true}, blank(id="filler"))
    ]
  )

# 1. `preempted` plays and is cut into by `at_boundary`  -> expect `false`.
# 2. `at_boundary` reaches the end of its track and is left -> expect `true`.
thread.run(delay=1., {use_boundary := true})
thread.run(
  delay=2.,
  {
    begin
      use_boundary := false
      use_preempted := false
    end
  }
)

thread.run(
  delay=4.,
  {
    begin
      log(
        "preempt_leave=#{preempt_leave()}, boundary_leave=#{boundary_leave()}"
      )
      if
        preempt_leave() == false and boundary_leave() == true
      then
        test.pass()
      else
        test.fail()
      end
    end
  }
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
