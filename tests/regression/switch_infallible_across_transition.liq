log.level.set(4)

# A switch with an infallible, always-eligible child claims to be infallible.
# It must stay ready across a full transition cycle, including the moment the
# transition graph returned by `on_select` runs out and the switch has to
# re-evaluate.

live = ref(false)
gap = ref(false)
frames = ref(0)

s =
  switch(
    [
      ({live()}, (sine(id="cutter") : source).{track_sensitive = false}),
      ({true}, blank(id="always"))
    ]
  )

# Flip back and forth so we cross both directions several times, each crossing
# building and then tearing down a transition.
s.on_frame(
  synchronous=true,
  fun () ->
    begin
      ref.incr(frames)
      if not source.is_ready(s) then gap := true end
      if frames() mod 60 == 0 then live := not live() end
      if frames() >= 900 then if gap() then test.fail() else test.pass() end end
    end
)

clock.assign_new(sync="none", [s])
output.dummy(s)
