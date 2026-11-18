%include "../test.liq"

# A skip asked from a callback of the running cycle changes nothing in that
# cycle: the track being started still delivers its frame and reports the
# same remaining time. The abort is carried out once the cycle is over, so
# the next track starts with the next frame.

count = ref(0)

def next() =
  ref.incr(count)
  title = "t#{count()}"
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    sine(duration=1.)
  )
end

s = source.dynamic(track_sensitive=true, next)

starts = ref([])
remaining_after_skip = ref(0.)

def on_track(m) =
  starts := [...starts(), (m["title"], source.time(s))]
  if
    m["title"] == "t1"
  then
    s.skip()
    remaining_after_skip := s.remaining()
  end

  if
    list.length(starts()) == 3
  then
    let (_, first) = list.nth(starts(), 0)
    let (second_title, second) = list.nth(starts(), 1)
    let (_, third) = list.nth(starts(), 2)
    frame = settings.frame.duration()

    if
      remaining_after_skip() < 0.5 or 1. < remaining_after_skip()
    then
      test.fail(
        "remaining right after the skip: #{remaining_after_skip()}s"
      )
    end

    if
      second_title != "t2" or abs(second - first - frame) > frame / 10.
    then
      test.fail(
        "#{second_title} started #{second - first}s after the skipped track"
      )
    end

    # The track that follows is not skipped.
    if
      abs(third - second - 1.) > 2. * frame
    then
      test.fail(
        "the track after the skip lasted #{third - second}s"
      )
    end

    test.pass()
  end
end

s.on_track(synchronous=true, on_track)
clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
