%include "../test.liq"

# Skipping a track must leave the next one audible.
tone = sine()
s = fade.out(track_sensitive=true, duration=0.04, tone)
s = rms(duration=0.2, s)
output.dummy(fallible=true, s)

thread.run(delay=0.5, tone.skip)
thread.run(
  delay=1.5,
  {if s.rms() > 0.01 then test.pass() else test.fail() end}
)
