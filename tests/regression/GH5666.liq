%include "../test.liq"

# A source preempted mid-track fades out under the starting source.

music = sine()

live_on = ref(false)
music.on_position(synchronous=true, position=1., fun (_, _) -> live_on := true)

live = source.available(blank(), live_on)

s = fallback([live.{track_sensitive = false}, music])
s = rms(duration=0.1, s)

# The starting source is silent: half way through the fade, all that is heard
# is the ending one.
s.on_position(
  synchronous=true,
  position=0.5,
  fun (_, _) ->
    if
      live_on()
    then
      level = s.rms()
      if
        0.1 < level and level < 0.6
      then
        test.pass()
      else
        test.fail(
          "unexpected level during the fade: #{level}"
        )
      end
    end
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
