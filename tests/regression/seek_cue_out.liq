%include "../test.liq"

# Seeking must move the cue-out point closer: a 2.5s track seeked forward by 1s
# plays for 1.5s.
s = single("annotate:liq_cue_in=0.5,liq_cue_out=3.:../streams/file1.mp3")

track_starts = ref([])

def seek() =
  let seeked = s.seek(1.)
  if seeked < 0.9 then test.fail() end
end

# Seeking before the track is open returns 0, and a seek from another thread can
# land past the cue-out point while the clock catches up.
def on_track(_) =
  track_starts := [...track_starts(), s.time()]
  if list.length(track_starts()) == 1 then seek() end
  if
    list.length(track_starts()) == 2
  then
    let [first, second] = track_starts()
    duration = second - first
    print(
      "First track lasted #{duration}s"
    )
    if 1.3 < duration and duration < 1.8 then test.pass() else test.fail() end
  end
end

s.on_track(synchronous=true, on_track)

output.dummy(s)
