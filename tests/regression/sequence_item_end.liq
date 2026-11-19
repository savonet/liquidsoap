%include "../test.liq"

# What finishes an item of a sequence, by kind of item. Only its track end,
# or its exhaustion, does: an item shorter than a frame plays in full, an
# item of several tracks gives one track unless `single_track` is unset, an
# item that never delivers is skipped, and an item that is only late is kept
# when nothing else can play and passed over when the last item can.

def tagged(title, duration) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    sine(duration=duration)
  )
end

def two_tracks() =
  sequence(single_track=false, [tagged("b1", 0.07), tagged("b2", 0.05)])
end

pending = ref(0)

def expect(~label, ~titles_expected, s) =
  ref.incr(pending)
  titles = ref([])
  s.on_track(synchronous=true, fun (m) -> titles := [...titles(), m["title"]])
  clock.assign_new(sync="none", [s])
  o = output.dummy(fallible=true, s)
  o.on_stop(
    synchronous=true,
    fun () ->
      begin
        if
          titles() != titles_expected
        then
          test.fail(
            "#{label}: #{titles()}"
          )
        end

        pending := pending() - 1
        if pending() == 0 then test.pass() end
      end
  )
end

# Shorter than a frame, never delivering, several tracks, then the last one.
expect(
  label="one track per item",
  titles_expected=["a", "b1", "c"],
  sequence([tagged("a", 0.005), source.fail(), two_tracks(), tagged("c", 0.1)])
)

expect(
  label="items played until exhausted",
  titles_expected=["a", "b1", "b2", "c"],
  sequence(
    single_track=false,
    [tagged("a", 0.005), source.fail(), two_tracks(), tagged("c", 0.1)]
  )
)

# Late at its turn, with a last item that never delivers: kept, then played.
opened = ref(false)
kept = sequence([source.available(tagged("late", 0.1), opened), source.fail()])
thread.run(delay=0.5, {opened := true})
expect(
  label="late item kept",
  titles_expected=["late"],
  kept
)

# Late at its turn, with a last item that can play: passed over.
never = ref(false)
expect(
  label="late item passed over",
  titles_expected=["last"],
  sequence([source.available(tagged("late", 0.1), never), tagged("last", 0.1)])
)
