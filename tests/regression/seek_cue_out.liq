%include "../test.liq"

# Seeking must move the cue-out point closer: a 2.5s track seeked forward by 1s
# plays for 1.5s.
s = single("annotate:liq_cue_in=0.5,liq_cue_out=3.:../streams/file1.mp3")

track_starts = ref([])

def on_track(_) =
  track_starts := [...track_starts(), s.time()]
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

thread.run(
  delay=0.5,
  {
    let seeked = s.seek(1.)
    if seeked < 0.9 then test.fail() end
  }
)

output.dummy(s)
