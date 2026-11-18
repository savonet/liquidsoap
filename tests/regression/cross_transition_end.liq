%include "../test.liq"

# When a transition is over. One made of several items plays them all, in
# order and in full, however short they are, and nothing marks the place
# where it gives way to the track. One that has nothing to deliver is
# replaced by its two sources, so that the stream never stops while the child
# has data and no sample of any track is lost.

rate = float(settings.frame.audio.samplerate())

def samples(seconds) =
  int(seconds * rate + 0.5)
end

pending = ref(0)

def finished() =
  pending := pending() - 1
  if pending() == 0 then test.pass() end
end

def record(s, on_done) =
  ref.incr(pending)
  out = file.temp(cleanup=true, "cross", ".wav")
  clock.assign_new(sync="none", [s])
  o = output.file(fallible=true, %wav(stereo = true), out, s)
  o.on_stop(
    synchronous=true,
    fun () ->
      begin
        on_done((file.size(out) - 44) / 4)
        finished()
      end
  )
end

# Four items, two of them well under a second: each crossing plays the last
# second of a track, 0.3 s, 0.4 s, then the first second of the next track.
tracks = [3., 3., 3.]
crossings = list.length(tracks) - 1

def four_items(ending, starting) =
  sequence(
    [
      ending.source,
      (sine(duration=0.3) : source),
      (sine(duration=0.4) : source),
      starting.source
    ]
  )
end

s =
  cross(
    duration=1.,
    deduplicate=false,
    four_items,
    sequence(list.map(fun (length) -> sine(duration=length), tracks))
  )

track_events = ref(0)
s.on_track(synchronous=true, fun (_) -> ref.incr(track_events))
record(
  s,
  fun (written) ->
    begin
      expected = samples(9.) + crossings * (samples(0.3) + samples(0.4))
      if
        written != expected
      then
        test.fail(
          "four items: #{written} samples written, expected #{expected}"
        )
      end

      # The first track, then one mark per item of each transition: none
      # where a transition gives way to the track.
      if
        track_events() != 1 + 4 * crossings
      then
        test.fail(
          "four items: #{track_events()} track events"
        )
      end
    end
)

# A long run of short tracks under a transition that returns, every other
# time, a source that delivers nothing.
short_tracks = list.init(40, fun (i) -> 0.3 + 0.07 * float(i mod 5))
calls = ref(0)
played_together = ref(0)

def sometimes_nothing(ending, starting) =
  ref.incr(calls)
  if
    calls() mod 2 == 0
  then
    source.fail()
  else
    played_together :=
      played_together() +
        min(
          samples(source.remaining(ending.source)),
          samples(source.remaining(starting.source))
        )
    add(normalize=false, [ending.source, starting.source])
  end
end

record(
  cross(
    duration=0.2,
    deduplicate=false,
    sometimes_nothing,
    sequence(list.map(fun (length) -> sine(duration=length), short_tracks))
  ),
  fun (written) ->
    begin
      total = list.fold(fun (n, length) -> n + samples(length), 0, short_tracks)

      if
        calls() != list.length(short_tracks) - 1
      then
        test.fail(
          "short tracks: #{calls()} transitions"
        )
      end

      if
        written != total - played_together()
      then
        test.fail(
          "short tracks: #{written} samples written, expected #{
            total - played_together()
          }"
        )
      end
    end
)
