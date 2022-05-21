# In #1146, we encountered a case where native flac
# decoding was stuck at the end of a track. This test
# makes sure we don't have such regression in the
# future.

track_count = ref(0)

s = single("../media/@flac[stereo].flac")

def f(_) =
  track_count := !track_count + 1

  if !track_count > 2 then
    test.pass()
  end
end

s.on_track(f)

clock.assign_new(sync="none",[s])

output.dummy(s)
