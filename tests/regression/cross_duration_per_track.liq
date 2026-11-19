%include "../test.liq"

# A track's cross duration governs the crossing at that track's end, and no
# other. An override carried by a track's metadata applies to that track
# only, unless `persist_override`; a `duration` function is read once per
# track.

rate = float(settings.frame.audio.samplerate())

def tagged(~override=null, title) =
  metadata.map(
    insert_missing=true,
    fun (_) ->
      [
        ("title", title),
        ...(
          null.defined(override)
            ? [("liq_cross_duration", "#{null.get(override)}")]
            : []
        )
      ],
    sine(duration=3.)
  )
end

pending = ref(0)

def check(~label, ~persist_override, ~overlaps_expected, ~durations_expected) =
  ref.incr(pending)
  asked = ref(0)
  def duration() =
    ref.incr(asked)
    1.
  end

  overlaps = ref([])
  def transition(ending, starting) =
    overlaps := [...overlaps(), source.remaining(ending.source)]
    add(normalize=false, [ending.source, starting.source])
  end

  s =
    cross(
      duration=duration,
      persist_override=persist_override,
      deduplicate=false,
      transition,
      sequence(
        [tagged("a"), tagged(override=0.5, "b"), tagged("c"), tagged("d")]
      )
    )

  # The duration of the track being buffered, read as each track starts.
  durations = ref([])
  s.on_track(
    synchronous=true,
    fun (_) -> durations := [...durations(), s.cross_duration()]
  )

  clock.assign_new(sync="none", [s])
  o = output.dummy(fallible=true, s)
  o.on_stop(
    synchronous=true,
    fun () ->
      begin
        def close(got, expected) =
          list.length(got) == list.length(expected)
          and list.for_all(
            fun (pair) -> abs(fst(pair) - snd(pair)) < 1. / rate,
            list.mapi(fun (i, x) -> (x, list.nth(expected, i)), got)
          )
        end

        if
          not close(overlaps(), overlaps_expected)
        then
          test.fail(
            "#{label}: ending sources of #{overlaps()}s"
          )
        end

        if
          not close(durations(), durations_expected)
        then
          test.fail(
            "#{label}: durations #{durations()}"
          )
        end

        # Once per track, when its first sample is buffered, unless an
        # override persisted from an earlier track is in force by then.
        asked_expected = persist_override ? 2 : 4
        if
          asked() != asked_expected
        then
          test.fail(
            "#{label}: `duration` read #{asked()} times"
          )
        end

        pending := pending() - 1
        if pending() == 0 then test.pass() end
      end
  )
end

# `b` is crossed over its own 0.5 s; `a` and `c` over the default second.
check(
  label="per track",
  persist_override=false,
  overlaps_expected=[1., 0.5, 1.],
  durations_expected=[1., 0.5, 1., 1.]
)

check(
  label="persisted",
  persist_override=true,
  overlaps_expected=[1., 0.5, 0.5],
  durations_expected=[1., 0.5, 0.5, 0.5]
)
