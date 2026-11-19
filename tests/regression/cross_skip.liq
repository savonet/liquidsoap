%include "../test.liq"

# A skip of a crossed source is heard at once: nothing of the skipped track
# is delivered after the cycle of the skip, the next track begins plainly,
# with a track event and its own metadata, and no transition is called for
# that boundary. This holds while a track plays, while a transition plays,
# and after the end of the track being heard was already found.

duration = 1.

# Tracks of 4 s titled `<prefix>1`, `<prefix>2`, ... and how many were asked.
def numbered() =
  count = ref(0)
  def next() =
    ref.incr(count)
    title = "t#{count()}"
    metadata.map(
      insert_missing=true,
      fun (_) -> [("title", title)],
      sine(duration=4.)
    )
  end

  (source.dynamic(self_sync=false, track_sensitive=true, next), count)
end

pending = ref(0)

# Skips once, `skip_at` seconds into the stream, and checks the tracks that
# begin, as (title, time), and the pairs of titles handed to the transition.
def check(~label, ~skip_at, ~tracks_expected, ~crossings_expected) =
  ref.incr(pending)
  let (child, _) = numbered()
  crossings = ref([])

  def transition(ending, starting) =
    crossings :=
      [...crossings(), (ending.metadata["title"], starting.metadata["title"])]
    add(normalize=false, [ending.source, starting.source])
  end

  s = cross(duration=duration, deduplicate=false, transition, child)
  tracks = ref([])
  skipped = ref(false)

  def on_track(m) =
    tracks := [...tracks(), (m["title"], source.time(s))]
    if
      list.length(tracks()) == list.length(tracks_expected)
    then
      got = tracks()
      ok =
        list.for_all(
          fun (pair) ->
            begin
              let ((title, at), (title_expected, at_expected)) = pair
              title == title_expected and abs(at - at_expected) <= 0.041
            end,
          list.mapi(fun (i, x) -> (x, list.nth(tracks_expected, i)), got)
        )

      if
        not ok
      then
        test.fail(
          "#{label}: tracks #{got}"
        )
      end
      if
        crossings() != crossings_expected
      then
        test.fail(
          "#{label}: crossings #{crossings()}"
        )
      end

      pending := pending() - 1
      if pending() == 0 then test.pass() end
    end
  end

  s.on_track(synchronous=true, on_track)
  s.on_frame(
    synchronous=true,
    fun () ->
      if
        not skipped() and skip_at <= source.time(s)
      then
        skipped := true
        s.skip()
      end
  )

  clock.assign_new(sync="none", [s])
  output.dummy(fallible=true, s)
end

# While `t1` plays, its end not found yet: the child is aborted, `t2` starts
# at once and the first crossing is the one from `t2` to `t3`, 3 s later.
check(
  label="in a track",
  skip_at=1.,
  tracks_expected=[("t1", 0.), ("t2", 1.), ("t3", 4.)],
  crossings_expected=[("t2", "t3")]
)

# During the transition from `t1` to `t2`, which starts at 3 s: for the
# listener `t2` began at the crossing, so `t2` is skipped and `t3` starts.
check(
  label="in a transition",
  skip_at=3.5,
  tracks_expected=[("t1", 0.), ("t2", 3.), ("t3", 3.5), ("t4", 6.5)],
  crossings_expected=[("t1", "t2"), ("t3", "t4")]
)

# At 2.5 s the end of `t1` was found, half a second before the crossing: the
# child is already on `t2`, which is kept and starts from its first sample,
# with no crossing out of the skipped track.
check(
  label="after the end was found",
  skip_at=2.5,
  tracks_expected=[("t1", 0.), ("t2", 2.5), ("t3", 5.5)],
  crossings_expected=[("t2", "t3")]
)
