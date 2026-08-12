log.level.set(4)

# Callbacks a script registers do not pile up on the sources they are
# registered on.
#
# `release()` takes a callback back, and an operator handing sources to a
# script function takes back what that function registered on them: `on_select`
# runs on every selection and registers on `base`, which is a child of the
# switch and outlives all of them. If those registrations were kept, one frame
# of `base` would fire the callback once per past selection instead of once.

released_hits = ref(0)
released = sine(id="released")
c =
  released.on_frame(
    synchronous=true,
    before=false,
    fun () -> ref.incr(released_hits)
  )
c.release()
c.release()

# Same, through the collector a script can use around its own functions. It
# takes several sources, and hands back what the function returned.
collected_hits = ref(0)
collected = sine(id="collected")
collected_too = sine(id="collected_too")

def register_on_collected() =
  collected.on_frame(
    synchronous=true,
    before=false,
    fun () -> ref.incr(collected_hits)
  )
  collected_too.on_frame(
    synchronous=true,
    before=false,
    fun () -> ref.incr(collected_hits)
  )
  "returned"
end

for _ = 1 to 3 do
  let {release, result} =
    source.collect_callback_releases(
      [collected, collected_too],
      register_on_collected
    )
  if result != "returned" then test.fail() end
  release()
end

base = sine(id="base")
other = sine(id="other")

# Registered once, never released: tells us how many frames `base` produced.
base_frames = ref(0)
base.on_frame(synchronous=true, before=false, fun () -> ref.incr(base_frames))

hits = ref(0)
selections = ref(0)

def on_select(
  ({starting}:{starting: source, ending: source?, replay_metadata: bool})
) =
  ref.incr(selections)
  base.on_frame(synchronous=true, before=false, fun () -> ref.incr(hits))
  starting
end

use_other = ref(false)

s =
  switch(
    [
      ({use_other()}, other.{track_sensitive = false}),
      ({true}, base.{track_sensitive = false, on_select = on_select})
    ]
  )

frames = ref(0)

s.on_frame(
  synchronous=true,
  before=false,
  fun () ->
    begin
      ref.incr(frames)

      # Flip every 10 frames, so `base` is selected again and again.
      if frames() mod 10 == 0 then use_other := not use_other() end

      if
        selections() >= 5 and frames() > 100
      then
        if
          released_hits() > 0 or collected_hits() > 0
        then
          log.important(
            "released callback still fired!"
          )
          test.fail()
        elsif
          hits() == 0 or base_frames() == 0
        then
          log.important(
            "callbacks never fired, test proves nothing"
          )
          test.fail()
        elsif
          # One live registration per frame of `base`, whatever the number of
          # selections. Leaked ones would multiply this by `selections()`.
          hits() > base_frames()
        then
          log.important(
            "#{hits()} hits for #{base_frames()} frames after #{selections()} \
             selections: callbacks piled up"
          )
          test.fail()
        else
          test.pass()
        end
      end
    end
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
