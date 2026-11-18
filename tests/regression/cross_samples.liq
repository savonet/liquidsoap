%include "../test.liq"

# Sample accounting through crossings. Whatever the lengths of the tracks
# against the cross duration, every sample of every track is heard exactly
# once outside the overlaps, both sources handed to the transition report
# their true length, the function is called at every boundary between two
# tracks and never for the last one.

rate = float(settings.frame.audio.samplerate())
sample = 1. / rate
duration = 1.

def samples(seconds) =
  int(seconds * rate + 0.5)
end

pending = ref(0)

# Crosses tones of the given lengths, in seconds, with a transition that
# adds its two sources, and checks the output and what the transition saw.
def check(~label, ~cross_duration=duration, ~overlaps_expected=null, lengths) =
  ref.incr(pending)
  lengths = (lengths : [float])
  child = sequence(list.map(fun (length) -> sine(duration=length), lengths))
  overlaps = ref([])
  played_together = ref(0)

  def transition(ending, starting) =
    ending_length = samples(source.remaining(ending.source))
    starting_length = samples(source.remaining(starting.source))
    overlaps := [...overlaps(), ending_length]
    played_together := played_together() + min(ending_length, starting_length)
    add(normalize=false, [ending.source, starting.source])
  end

  s = cross(duration=cross_duration, deduplicate=false, transition, child)
  out = file.temp(cleanup=true, "cross", ".wav")
  clock.assign_new(sync="none", [s])
  o = output.file(fallible=true, %wav(stereo = true), out, s)
  o.on_stop(
    synchronous=true,
    fun () ->
      begin
        total = list.fold(fun (n, length) -> n + samples(length), 0, lengths)
        written = (file.size(out) - 44) / 4

        if
          list.length(overlaps()) != list.length(lengths) - 1
        then
          test.fail(
            "#{label}: #{list.length(overlaps())} transitions for #{
              list.length(lengths)
            } tracks"
          )
        end

        if
          written != total - played_together()
        then
          test.fail(
            "#{label}: #{written} samples written, expected #{
              total - played_together()
            } (#{total} in the tracks, #{played_together()} played together)"
          )
        end

        if
          null.defined(overlaps_expected)
          and overlaps() != null.get(overlaps_expected)
        then
          test.fail(
            "#{label}: ending sources of #{overlaps()} samples, expected #{
              null.get(overlaps_expected)
            }"
          )
        end

        pending := pending() - 1
        if pending() == 0 then test.pass() end
      end
  )
end

# Tracks of half, once and three times the duration, in every order.
half = duration / 2.
triple = 3. * duration
list.iter(
  fun (lengths) ->
    check(
      label="order #{lengths}",
      lengths
    ),
  [
    [half, duration, triple],
    [half, triple, duration],
    [duration, half, triple],
    [duration, triple, half],
    [triple, half, duration],
    [triple, duration, half]
  ]
)

# The same with lengths that fall inside a frame.
check(
  label="off the frame",
  [triple + 7. * sample, half + sample, triple - sample]
)

# A middle track exactly as long as the window is used whole by the crossing
# that starts it, like one a sample shorter: the crossing at its end has
# nothing of it left. One a sample longer has that sample left.
whole = samples(duration)
check(
  label="exactly the window",
  overlaps_expected=[whole, 0],
  [triple, duration, triple]
)
check(
  label="a sample shorter",
  overlaps_expected=[whole - 1, 0],
  [triple, duration - sample, triple]
)
check(
  label="a sample longer",
  overlaps_expected=[whole, 1],
  [triple, duration + sample, triple]
)

# No overlap: the function is still called at every boundary, with empty
# sources, and nothing is lost.
check(
  label="zero",
  cross_duration=0.,
  overlaps_expected=[0, 0],
  [triple, half, triple]
)
check(
  label="negative",
  cross_duration=-1.,
  overlaps_expected=[0, 0],
  [triple, half, triple]
)
check(
  label="one sample",
  cross_duration=sample,
  overlaps_expected=[1, 1],
  [triple, half, triple]
)
