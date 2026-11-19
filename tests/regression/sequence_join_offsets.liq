%include "../test.liq"

# A join between two items, wherever it falls in a frame: on a frame edge,
# one sample before or after it, in the middle of a frame, and for an item
# shorter than a frame. The sequence plays exactly the samples of its two
# items, no more and no less; entered as a track the second item is marked
# once, and merged it is not marked and keeps its metadata.

frame = settings.frame.duration()
sample = 1. / float(settings.frame.audio.samplerate())

first_durations =
  [5. * frame, 5. * frame + sample, 5. * frame - sample, 5.5 * frame, frame / 4.
  ]

second_duration = 7.3 * frame

def tagged(title, duration) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    sine(duration=duration)
  )
end

pending = ref(0)

# Run once every recording has stopped: they stop in no particular order.
checks = ref([])

# Plays `s` to a file and gives its size once everything has stopped.
def record(s, on_done) =
  ref.incr(pending)
  out = file.temp(cleanup=true, "join", ".wav")
  checks := [...checks(), fun () -> on_done(file.size(out))]
  clock.assign_new(sync="none", [s])
  o = output.file(fallible=true, %wav, out, s)
  o.on_stop(
    synchronous=true,
    fun () ->
      begin
        pending := pending() - 1
        if
          pending() == 0
        then
          list.iter(fun (check) -> check(), checks())
          test.pass()
        end
      end
  )
end

def check(first_duration) =
  alone = ref(0)
  record(tagged("first", first_duration), fun (size) -> alone := size)

  second_alone = ref(0)
  record(tagged("second", second_duration), fun (size) -> second_alone := size)

  def joined(~merge, ~tracks_expected) =
    s =
      sequence(
        merge=merge,
        [tagged("first", first_duration), tagged("second", second_duration)]
      )

    tracks = ref([])
    titles = ref([])
    s.on_track(synchronous=true, fun (m) -> tracks := [...tracks(), m["title"]])
    s.on_metadata(
      synchronous=true,
      fun (m) -> titles := [...titles(), m["title"]]
    )

    record(
      s,
      fun (size) ->
        begin
          label =
            "first item of #{first_duration}s, merge=#{merge}"
          if
            tracks() != tracks_expected
          then
            test.fail(
              "#{label}: tracks #{tracks()}"
            )
          end

          if
            titles() != ["first", "second"]
          then
            test.fail(
              "#{label}: metadata #{titles()}"
            )
          end

          header = 44
          expected = alone() + second_alone() - header
          if
            size != expected
          then
            test.fail(
              "#{label}: #{size} bytes, expected #{expected}"
            )
          end
        end
    )
  end

  joined(merge=false, tracks_expected=["first", "second"])
  joined(merge=true, tracks_expected=["first"])
end

list.iter(check, first_durations)
