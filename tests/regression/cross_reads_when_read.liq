%include "../test.liq"

# `cross` reads its child, and runs the transition function, only while it
# is read. Asking whether it is ready does neither: probed ten times in every
# cycle it enters the same tracks of its child at the same moments and calls
# the function as many times. And it reads no further ahead than one cross
# duration plus one frame.

duration = 1.
track_length = 4.
tracks_wanted = 5
finished = ref([])

def run(~probed) =
  # When each track of the child was entered, in time of the stream.
  entered = ref([])
  stream = ref(fun () -> 0.)
  calls = ref(0)

  def next() =
    now = stream()
    entered := [...entered(), now()]
    sine(duration=track_length)
  end

  child = source.dynamic(self_sync=false, track_sensitive=true, next)

  def transition(ending, starting) =
    ref.incr(calls)
    add(normalize=false, [ending.source, starting.source])
  end

  s = cross(duration=duration, deduplicate=false, transition, child)
  stream := fun () -> source.time(s)
  tracks = ref(0)

  def on_track(_) =
    ref.incr(tracks)
    if
      tracks() == tracks_wanted
    then
      finished := [...finished(), (entered(), calls())]
      if
        list.length(finished()) == 2
      then
        let (first_entered, first_calls) = list.nth(finished(), 0)
        let (second_entered, second_calls) = list.nth(finished(), 1)
        test.equal(first_calls, second_calls)
        test.equal(first_entered, second_entered)

        # A crossing starts 3 s after the previous one. The child enters the
        # next track when the end of the current one is found: one duration
        # and one frame before it, and no earlier.
        frame = settings.frame.duration()
        list.iteri(
          fun (i, at) ->
            if
              0 < i
            then
              crossing = track_length - duration + 3. * float(i - 1)
              if
                at < crossing - duration - 2. * frame or crossing < at
              then
                test.fail(
                  "track #{i + 1} of the child entered at #{at}s, its crossing \
                   is at #{crossing}s"
                )
              end
            end,
          first_entered
        )

        test.pass()
      end
    end
  end

  s.on_track(synchronous=true, on_track)

  if
    probed
  then
    s.on_frame(
      synchronous=true,
      fun () ->
        list.iter(fun (_) -> ignore(s.is_ready()), list.init(10, fun (i) -> i))
    )
  end

  clock.assign_new(sync="none", [s])
  output.dummy(fallible=true, s)
end

run(probed=false)
run(probed=true)
