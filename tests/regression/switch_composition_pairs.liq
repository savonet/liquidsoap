%include "../test.liq"

# The four rows of the composition table, with the default handoff:
# - file to file: the switch waits for the end of the track;
# - file to live: it cuts, and the file track that was cut is aborted;
# - live to file: it cuts, and the live source resumes where it was;
# - out of a source that has nothing to deliver: at once, whatever it is.

# Tracks titled `<prefix>1`, `<prefix>2`, ... of a fixed duration.
def numbered(~duration, prefix) =
  count = ref(0)
  def next() =
    ref.incr(count)
    title = "#{prefix}#{count()}"
    metadata.map(
      insert_missing=true,
      fun (_) -> [("title", title)],
      sine(duration=duration)
    )
  end

  source.dynamic(track_sensitive=true, next)
end

def live(~duration, prefix) =
  s = numbered(duration=duration, prefix)
  s.composition_type := "live"
  s
end

frame = settings.frame.duration()
pending = ref(0)

# Calls `script(frames)` before each frame of a source and checks the tracks
# it starts, as (title, time), once `tracks_wanted` of them started. The
# source is given by its methods: a function cannot use several methods of a
# source it takes as argument.
def row(~label, ~tracks_wanted, ~script, ~check, ~on_frame, ~on_track, ~now) =
  ref.incr(pending)
  frames = ref(0)
  starts = ref([])
  register_on_track = on_track
  on_frame(
    synchronous=true,
    fun () ->
      begin
        ref.incr(frames)
        script(frames())
      end
  )

  def on_track(m) =
    starts := [...starts(), (m["title"], now())]
    if
      list.length(starts()) == tracks_wanted
    then
      problem = check(starts())
      if
        null.defined(problem)
      then
        test.fail(
          "#{label}: #{problem} in #{starts()}"
        )
      end

      pending := pending() - 1
      if pending() == 0 then test.pass() end
    end
  end

  register_on_track(synchronous=true, on_track)
end

def play(s) =
  clock.assign_new(sync="none", [s])
  output.dummy(fallible=true, s)
end

def title(starts, index) =
  fst(list.nth(starts, index))
end

def time(starts, index) =
  snd(list.nth(starts, index))
end

def near(x, y) =
  abs(x - y) <= 3. * frame
end

# File to file: asked for at 0.3 s, `b` starts where `a1` ends, at 1 s.
due = ref(false)
s1 =
  switch(
    [(due, numbered(duration=1., "b")), ({true}, numbered(duration=1., "a"))]
  )
row(
  label="file to file",
  tracks_wanted=2,
  script=fun (frames) -> if frames == 15 then due := true end,
  check=fun (starts) ->
    if
      title(starts, 1) != "b1" or not near(time(starts, 1), 1.)
    then
      "the second track should be b1 at 1 s"
    else
      null
    end,
  on_frame=s1.on_frame,
  on_track=s1.on_track,
  now={source.time(s1)}
)
play(s1)

# File to live: the live source cuts in at 0.3 s. `a1` has more left than the
# fade lasts, so it is aborted: back on `a`, the track is `a2`.
due = ref(false)
s2 =
  switch([(due, live(duration=10., "l")), ({true}, numbered(duration=5., "a"))])
row(
  label="file to live",
  tracks_wanted=3,
  script=fun (frames) ->
    begin
      if frames == 15 then due := true end
      if frames == 90 then due := false end
    end,
  check=fun (starts) ->
    if
      title(starts, 1) != "l1" or not near(time(starts, 1), 0.3)
    then
      "the second track should be l1 at 0.3 s"
    elsif
      title(starts, 2) != "a2"
    then
      "the track after the live source should be a2"
    else
      null
    end,
  on_frame=s2.on_frame,
  on_track=s2.on_track,
  now={source.time(s2)}
)
play(s2)

# Live to file: the file source cuts in at 0.3 s. At 1.5 s, once the live
# source has faded out, it is not asked for any more and the live source cuts
# back in. It was not aborted: the track is `l1` again.
due = ref(false)
s3 =
  switch(
    [(due, numbered(duration=10., "a")), ({true}, live(duration=10., "l"))]
  )
row(
  label="live to file",
  tracks_wanted=3,
  script=fun (frames) ->
    begin
      if frames == 15 then due := true end
      if frames == 75 then due := false end
    end,
  check=fun (starts) ->
    if
      title(starts, 1) != "a1" or not near(time(starts, 1), 0.3)
    then
      "the second track should be a1 at 0.3 s"
    elsif
      title(starts, 2) != "l1" or not near(time(starts, 2), 1.5)
    then
      "the third track should be l1 again at 1.5 s"
    else
      null
    end,
  on_frame=s3.on_frame,
  on_track=s3.on_track,
  now={source.time(s3)}
)
play(s3)

# Out of a source that has nothing left to deliver: `b` starts at once, at
# 0.3 s, although `a1` had not ended.
available = ref(true)
s4 =
  fallback(
    [
      source.available(numbered(duration=5., "a"), available),
      numbered(duration=1., "b")
    ]
  )

row(
  label="nothing to deliver",
  tracks_wanted=2,
  script=fun (frames) -> if frames == 15 then available := false end,
  check=fun (starts) ->
    if
      title(starts, 1) != "b1" or not near(time(starts, 1), 0.3)
    then
      "the second track should be b1 at 0.3 s"
    else
      null
    end,
  on_frame=s4.on_frame,
  on_track=s4.on_track,
  now={source.time(s4)}
)
play(s4)
