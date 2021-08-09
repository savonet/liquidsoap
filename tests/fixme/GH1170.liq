#!../src/liquidsoap ../libs/stdlib.liq ../libs/deprecations.liq

%include "test.liq"

# GH1170 was a tricky bug: we were checking on [is_ready]
# for the crossfaded source only based on the transition source
# during transitions. However, if the transition source fails,
# we should still be able to switch back to the main source if
# it is ready..

s = noise()

s = chop(duration=20., s)

track_count = ref 0

def f(_) =
  track_count := !track_count + 1
  # Let's do 3 tracks here for security.
  if !track_count > 2 then
    test.pass()
  end
end

s = on_track(f,s)

s = crossfade(s)

clock.assign_new(sync="none",[s])

def on_stop() =
  if !track_count < 3 then
    test.fail()
  end
end

output.dummy(on_stop=on_stop, fallible=true, s)
