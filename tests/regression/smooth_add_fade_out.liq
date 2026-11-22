%include "../test.liq"

# smooth_add used to cut the special source at full volume when its track ended
# by itself, instead of fading it out (#3714).

log.level := 4
pending = ref(2)

def done(ok) =
  if
    ok
  then
    pending := pending() - 1
    if pending() == 0 then test.pass() end
  else
    test.fail()
  end
end

# Runs `smooth_add` with a silent normal source, so that the output is the
# special source alone, and calls `check` with the output RMS in 0.1s windows,
# as (time, level) pairs.
def run(name, (special_duration:float), check) =
  special = once(sine(duration=special_duration, 880.))
  s = smooth_add(duration=1., p=0.2, normal=blank(), special=special)
  s = rms(duration=0.1, s)
  levels = ref([])
  checked = ref(false)

  def on_frame() =
    levels := [(s.time(), s.rms()), ...levels()]
    if
      not checked() and s.time() > special_duration + 1.
    then
      checked := true
      levels = list.rev(levels())
      print(
        "#{name}: #{list.map(fun (e) -> int_of_float(100. * snd(e)), levels)}"
      )

      done(check(levels))
    end
  end

  s.on_frame(synchronous=true, on_frame)
  clock.assign_new(sync="none", [s])
  output.dummy(fallible=true, s)
end

def level_at(levels, t) =
  def closest(acc, e) =
    if abs(fst(e) - t) < abs(fst(acc) - t) then e else acc end
  end

  snd(list.fold(closest, (-1., 0.), levels))
end

# A 0.8 * sine has an RMS of about 0.57: the special plays at full level in the
# middle and is mostly faded out 0.2s before its end.
run(
  "long",
  4.,
  fun (levels) ->
    begin
      full = level_at(levels, 2.)
      full > 0.4 and level_at(levels, 3.8) < full / 3.
    end
)

# A special track shorter than twice the fade duration fades out from the level
# its fade in reached: the level goes up, then only down.
run(
  "short",
  1.5,
  fun (levels) ->
    begin
      late = level_at(levels, 1.45)
      levels = list.map(snd, levels)

      def step(acc, level) =
        let (previous, went_down, ok) = acc
        went_down = went_down or level < previous - 0.01
        (level, went_down, ok and not (went_down and level > previous + 0.01))
      end

      let (_, went_down, ok) = list.fold(step, (0., false, true), levels)
      went_down and ok and late < 0.05
    end
)
