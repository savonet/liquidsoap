fade = ()

let settings.fade =
  settings.make.void(
    "Settings for the fade in/out operators"
  )

let settings.fade.in =
  settings.make.void(
    "Settings for fade.in operators"
  )

let settings.fade.in.duration =
  settings.make(
    description="Default fade.in duration",
    3.
  )

let settings.fade.in.type =
  settings.make(
    description="Default fade.in type",
    "lin"
  )

let settings.fade.in.curve =
  settings.make(
    description="Default fade.in curve",
    10.
  )

let settings.fade.out =
  settings.make.void(
    "Settings for fade.out operators"
  )

let settings.fade.out.duration =
  settings.make(
    description="Default fade.out duration",
    3.
  )

let settings.fade.out.type =
  settings.make(
    description="Default fade.out type",
    "lin"
  )

let settings.fade.out.curve =
  settings.make(
    description="Default fade.out curve",
    10.
  )

# Make a fade function based on a source's clock.
# @category Source / Fade
# @param ~curve Fade curve for `"log"` and `"exp"` shapes. If `null`, depends on the type of fade. \
#               The higher the value, the shaper the curve.
# @param ~type Fade shape. One of: `"sin"`, `"exp"`, `"log"`, `"lin"`
# @param ~start Start value.
# @param ~stop Stop value.
# @param ~delay Initial delay before starting fade.
# @param ~duration Duration in seconds.
# @param ~on_done Function to execute when the fade is finished
def mkfade(
  ~curve=null,
  ~type="lin",
  ~start=0.,
  ~stop=1.,
  ~delay=0.,
  ~duration=3.,
  ~on_done={()},
  s
) =
  def log(x) =
    log(label="mkfade", x)
  end

  # Shape functions must map 0. -> 0. and 1. -> 1.
  pi = acos(-1.)

  def sin_shape(x) =
    (1. + sin((x - 0.5) * pi)) / 2.
  end

  exp_curve = curve ?? 3.
  m = exp(exp_curve - 1.) - exp(-1.)

  def exp_shape(x) =
    (exp((exp_curve * x) - 1.) - exp(-1.)) / m
  end

  ln_curve = curve ?? 10.
  m = ln(1. + ln_curve)

  def log_shape(x) =
    ln(1. + ln_curve * x) / m
  end

  def lin_shape(x) =
    x
  end

  shape =
    if
      type == "sin"
    then
      sin_shape
    elsif type == "exp" then exp_shape
    elsif type == "log" then log_shape
    elsif type == "lin" then lin_shape
    else
      log(
        "Invalid type #{type}, using \"lin\""
      )
      lin_shape
    end

  start_time = ref(-1.)
  increasing_fade = start <= stop

  def fade() =
    if start_time() < 0. then start_time := source.time(s) end

    t = source.time(s) - start_time() - delay
    if
      0. < duration and t <= 0.
    then
      start
    elsif
      t >= duration
    then
      on_done()
      stop
    else
      if
        increasing_fade
      then
        start + shape(t / duration) * (stop - start)
      else
        stop + shape(1. - t / duration) * (start - stop)
      end
    end
  end

  fade
end

# Scale source during fading.
# @category Source / Fade
# @flag hidden
def fade.scale(~id="fade.scale", x, s) =
  amplify(id=id, override=null, x, s)
end

# Fade the end of tracks.
# @category Source / Fade
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override. Defaults to `settings.fade.out.curve` if `null`.
# @param ~delay Initial delay before starting fade. Defaults to `settings.fade.out.delay` if `null`.
# @param ~curve Fade curve. Defaults to `settings.fade.out.curve` if `null`.
# @param ~override_duration Metadata field which, if present and containing a float, overrides the 'duration' parameter for the current track.
# @param ~override_type Metadata field which, if present and correct, overrides the 'type' parameter for the current track.
# @param ~override_curve Metadata field which, if presents and correct, overrides the `curve` parameter for the current track. Use `"default"` \
#                        to set to default value.
# @param ~override_delay Metadata field which, if presents and correct, overrides the initial fade delay.
# @param ~persist_overrides Keep duration and type overrides on track change.
# @param ~track_sensitive Be track sensitive (if `false` we only fade ou once at the beginning of the track).
# @param ~initial_metadata Initial metadata.
# @param ~type Fader shape. One of: "lin"", "sin", "log" or "exp". Defaults to `settings.fade.out.type` if `null`.
def fade.out(
  ~id="fade.out",
  ~duration=null,
  ~delay=0.,
  ~curve=null,
  ~override_duration="liq_fade_out",
  ~override_type="liq_fade_out_type",
  ~override_curve="liq_fade_out_curve",
  ~override_delay="liq_fade_out_delay",
  ~persist_overrides=false,
  ~track_sensitive=false,
  ~initial_metadata=[],
  ~type=null,
  s
) =
  def log(~level=4, x) =
    log(label=source.id(s), level=level, x)
  end

  fn = ref(fun () -> 1.)
  original_type = type ?? settings.fade.out.type()
  type = ref(original_type)
  original_curve = (curve ?? settings.fade.out.curve() : float?)
  curve = ref(original_curve)
  original_duration = duration ?? settings.fade.out.duration()
  duration = ref(original_duration)
  original_delay = delay
  delay = ref(original_delay)
  start_time = ref(-1.)
  started = ref(false)
  last_metadata = ref(initial_metadata)

  def start_fade(d, _) =
    curve_log =
      if null.defined(curve()) then string(null.get(curve())) else "default" end

    start_time := source.time(s)
    let (delay, duration) =
      if
        d < delay() + duration()
      then
        if
          d < duration()
        then
          (0., d)
        else
          (max(0., d - duration()), duration())
        end
      else
        (delay(), duration())
      end

    log(
      "Fading out with type: #{type()}, curve: #{curve_log}, delay: #{delay}s, \
       duration: #{duration}s and #{d}s remaining."
    )

    fn :=
      mkfade(
        start=1.,
        stop=0.,
        type=type(),
        curve=curve(),
        delay=delay,
        duration=duration,
        s
      )
    started := true
  end

  def apply() =
    fn = fn()
    fn()
  end

  def stop_fade(_) =
    if
      started()
    then
      fn := fun () -> 1.
      started := false
    end
  end

  def update_fade(m) =
    if
      m[override_duration] != ""
    then
      old_duration = duration()
      duration := float_of_string(default=duration(), m[override_duration])

      if
        duration() != old_duration
      then
        log(
          "New fade out duration: #{duration()}s."
        )
      end
    end

    if
      m[override_delay] != ""
    then
      old_delay = delay()
      delay := float_of_string(default=0., m[override_delay])

      if
        delay() != old_delay
      then
        log(
          "New fade out delay: #{delay()}s."
        )
      end
    end

    if
      m[override_curve] != ""
    then
      old_curve = curve()

      if
        m[override_curve] == "default"
      then
        curve := null
      else
        try
          curve := float_of_string(m[override_curve])
        catch _ do
          curve := null
        end
      end

      if
        curve() != old_curve
      then
        log(
          "New fade out curve: #{curve()}."
        )
      end
    end

    if
      m[override_type] != ""
    then
      old_type = type()
      type := m[override_type]

      if
        type() != old_type
      then
        log(
          "New fade out type: #{type()}."
        )
      end
    end
  end

  update_fade(last_metadata())

  def reset_overrides(_) =
    if
      not persist_overrides
    then
      log(
        "Setting fade out to default duration: #{original_duration}s, delay: #{
          original_delay
        }s, type: #{original_type}, curve: #{original_curve}"
      )
      duration := original_duration
      delay := original_delay
      type := original_type
      curve := original_curve
    end
  end

  s = source.methods(s)
  s.on_track(synchronous=true, reset_overrides)
  s.on_metadata(synchronous=true, update_fade)
  if track_sensitive then s.on_track(synchronous=true, stop_fade) end

  d = source.dynamic(memoize({s}))

  if
    track_sensitive
  then
    d.on_position(
      synchronous=true,
      remaining=true,
      position=duration,
      start_fade
    )
  else
    d.on_frame(
      synchronous=true,
      before=false,
      memoize({start_fade(source.remaining(s), [])})
    )
  end

  # Make sure we generate a frame before starting the fade
  # to ensure that we catch all the initial metadata.
  d.on_frame(
    synchronous=true,
    before=true,
    memoize({if s.is_ready() then s.generate_frame() end})
  )

  fade.scale(id=id, apply, d).{fade_duration={duration()}, fade_type={type()}}
end

# Fade when the metadata trigger is received and then skip.
# @flag extra
# @category Source / Fade
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
# @param ~delay Initial delay before starting fade.
# @param ~curve Fade curve. Default if `null`.
# @param ~override_duration Metadata field which, if present and containing a float, overrides the 'duration' parameter for the current track.
# @param ~override_type Metadata field which, if present and correct, overrides the 'type' parameter for the current track.
# @param ~override_curve Metadata field which, if presents and correct, overrides the `curve` parameter for the current track. Use `"default"` \
#                        to set to default value.
# @param ~override_skip Metadata field which, when present and set to "true", will trigger the fade
# @param ~persist_overrides Keep duration and type overrides on track change.
# @param ~initial_metadata Initial metadata.
# @param ~type Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
def fade.skip(
  ~id="fade.skip",
  ~duration=5.,
  ~delay=0.,
  ~curve=null,
  ~override_duration="liq_fade_skip",
  ~override_type="liq_fade_skip_type",
  ~override_curve="liq_fade_skip_curve",
  ~persist_overrides=false,
  ~override_skip="liq_skip_meta",
  ~initial_metadata=[],
  ~type="lin",
  s
) =
  def log(x) =
    log(label=source.id(s), level=4, x)
  end

  fn = ref(fun () -> 1.)
  original_type = type
  type = ref(type)
  original_curve = curve
  curve = ref(curve)
  original_duration = duration
  duration = ref(duration)
  original_delay = delay
  delay = ref(original_delay)
  last_metadata = ref(initial_metadata)

  def apply() =
    fn = fn()
    fn()
  end

  def stop_fade(_) =
    fn := fun () -> 1.
  end

  def skip() =
    log(
      "Fade finished executing. Calling skip now"
    )
    source.skip(s)
  end

  def update_fade(m) =
    if
      m[override_skip] == "true"
    then
      remaining = source.remaining(s)
      duration = if remaining < duration() then remaining else duration() end
      log(
        "Skip fade executed for: #{duration}s"
      )
      fn :=
        mkfade(
          start=1.,
          stop=0.,
          type=type(),
          curve=curve(),
          duration=duration,
          delay=delay(),
          on_done=skip,
          s
        )
    end

    if
      m[override_duration] != ""
    then
      old_duration = duration()
      duration := float_of_string(default=duration(), m[override_duration])

      if
        duration() != old_duration
      then
        log(
          "New fade skip duration: #{duration()}s."
        )
      end
    end

    if
      m[override_curve] != ""
    then
      old_curve = curve()

      if
        m[override_curve] == "default"
      then
        curve := null
      else
        try
          curve := float_of_string(m[override_curve])
        catch _ do
          curve := null
        end
      end

      if
        curve() != old_curve
      then
        log(
          "New fade skip curve: #{curve()}."
        )
      end
    end

    if
      m[override_type] != ""
    then
      old_type = type()
      type := m[override_type]

      if
        type() != old_type
      then
        log(
          "New fade skip type: #{type()}."
        )
      end
    end
  end

  update_fade(last_metadata())

  def reset_overrides(_) =
    if
      not persist_overrides
    then
      log(
        "Setting fade skip to default duration: #{original_duration}s, delay: #{
          original_delay
        }s, type: #{original_type}, curve: #{original_curve}"
      )
      duration := original_duration
      type := original_type
      curve := original_curve
    end
  end

  s = source.methods(s)
  s.on_track(synchronous=true, reset_overrides)
  s.on_metadata(synchronous=true, update_fade)
  s.on_track(synchronous=true, stop_fade)

  fade.scale(id=id, apply, s).{fade_duration={duration()}, fade_type={type()}}
end

# Fade the beginning of tracks.
# @category Source / Fade
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override. Defaults to `settings.fade.in.duration` if `null`.
# @param ~delay Initial delay before starting fade.
# @param ~curve Fade curve. Defaults to `settings.fade.in.curve` if `null`.
# @param ~override_duration Metadata field which, if present and containing a float, overrides the 'duration' parameter for the current track.
# @param ~override_type Metadata field which, if present and correct, overrides the 'type' parameter for the current track.
# @param ~override_curve Metadata field which, if presents and correct, overrides the `curve` parameter for the current track. Use `"default"` \
#                        to set to default value.
# @param ~override_delay Metadata field which, if presents and correct, overrides the initial fade delay.
# @param ~persist_overrides Keep duration and type overrides on track change.
# @param ~track_sensitive Be track sensitive (if `false` we only fade in once at the beginning of the track).
# @param ~initial_metadata Initial metadata.
# @param ~type Fader shape. One of: "lin"", "sin", "log" or "exp". Defaults to `settings.fade.in.type` if `null`.
def fade.in(
  ~id="fade.in",
  ~duration=null,
  ~delay=0.,
  ~curve=null,
  ~override_duration="liq_fade_in",
  ~override_type="liq_fade_in_type",
  ~override_curve="liq_fade_in_curve",
  ~override_delay="liq_fade_in_delay",
  ~persist_overrides=false,
  ~track_sensitive=false,
  ~initial_metadata=[],
  ~type=null,
  s
) =
  def log(~level=4, x) =
    log(label=source.id(s), level=level, x)
  end

  fn = ref(fun () -> 0.)
  original_duration = duration ?? settings.fade.in.duration()
  duration = ref(original_duration)
  original_delay = delay
  delay = ref(original_delay)
  original_type = type ?? settings.fade.in.type()
  type = ref(original_type)
  original_curve = curve ?? settings.fade.in.curve()
  curve = ref(curve)
  last_metadata = ref(initial_metadata)

  def apply() =
    fn = fn()
    fn()
  end

  def start_fade(_) =
    curve_log =
      if null.defined(curve()) then string(null.get(curve())) else "default" end

    duration =
      if
        source.remaining(s) < duration()
      then
        source.remaining(s)
      else
        duration()
      end

    log(
      "Fading in with type: #{type()}, curve: #{curve_log}, delay: #{delay()}s \
       and duration: #{duration}s."
    )

    # Note: delay here must match the add delay via blank to make the curve also match
    fn :=
      mkfade(
        start=0.,
        stop=1.,
        type=type(),
        curve=curve(),
        delay=delay(),
        duration=duration,
        s
      )
  end

  def update_fade(m) =
    if
      m[override_duration] != ""
    then
      old_duration = duration()
      duration := float_of_string(default=duration(), m[override_duration])

      if
        duration() != old_duration
      then
        log(
          "New fade in duration: #{duration()}s."
        )
      end
    end

    if
      m[override_delay] != ""
    then
      old_delay = delay()
      delay := float_of_string(default=0., m[override_delay])

      if
        delay() != old_delay
      then
        log(
          "New fade in delay: #{delay()}s."
        )
      end
    end

    if
      m[override_curve] != ""
    then
      old_curve = curve()

      if
        m[override_curve] == "default"
      then
        curve := null
      else
        try
          curve := float_of_string(m[override_curve])
        catch _ do
          curve := null
        end
      end

      if
        curve() != old_curve
      then
        log(
          "New fade in curve: #{curve()}."
        )
      end
    end

    if
      m[override_type] != ""
    then
      old_type = type()
      type := m[override_type]

      if
        type() != old_type
      then
        log(
          "New fade in type: #{type()}."
        )
      end
    end
  end

  update_fade(last_metadata())

  def reset_overrides(_) =
    if
      not persist_overrides
    then
      log(
        "Setting fade in to default duration: #{original_duration}s, delay: #{
          original_delay
        }s, type: #{original_type}, curve: #{original_curve}"
      )
      duration := original_duration
      delay := original_delay
      type := original_type
      curve := original_curve
    end
  end

  s = source.methods(s)
  s.on_track(synchronous=true, reset_overrides)
  s.on_metadata(synchronous=true, update_fade)

  # Initial duration for fade.in should be computed after fetching the initial
  # frame from s to make sure any initial metadata is taken into account
  prepare = ref(fun (_) -> ())
  s =
    source.dynamic(
      memoize(
        fun () ->
          begin
            prepare = prepare()
            prepare(s)
            if s.is_active() then s.generate_frame() end
            delay = delay()
            if
              delay > 0.
            then
              effective_source = source.methods(source.effective(s))
              if
                s.is_active()
              then
                log(
                  level=2,
                  "Active source #{effective_source.id()} is faded in after an \
                   initial delay of #{delay}s. This will result is initial data \
                   loss since the source will be animated while the delay \
                   expires."
                )
              end
              sequence(merge=true, [blank(duration=delay), s])
            else
              s
            end
          end
      )
    )

  s = fade.scale(id=id, apply, s)

  if
    track_sensitive
  then
    s.on_track(synchronous=true, start_fade)
  else
    s.on_frame(synchronous=true, before=false, memoize({start_fade([])}))
  end
  s.{fade_duration={duration()}, fade_delay={delay()}, fade_type={type()}}
end

# Simple transition for crossfade
# @category Source / Fade
# @param ~log Logging utility
# @param ~fade_in  Fade-in duration, if any.
# @param ~fade_out Fade-out duration, if any.
# @param ~initial_fade_in_metadata Initial fade-in metadata
# @param ~initial_fade_out_metadata Initial fade-out metadata
# @param ~ending_map Optional mapping for the ending track
# @param ~starting_map Optional mapping for the starting track
# @param a Ending track
# @param b Starting track
def cross.simple(
  ~log=(fun (s) -> log(label="cross.simple", level=3, s)),
  ~fade_in=3.,
  ~fade_out=3.,
  ~initial_fade_in_metadata=[],
  ~initial_fade_out_metadata=[],
  ~ending_map=fun (s) -> s,
  ~starting_map=fun (s) -> s,
  a,
  b
) =
  fade_out_start_next =
    if
      list.assoc.mem("liq_fade_out_start_next", initial_fade_out_metadata)
    then
      float_of_string(initial_fade_out_metadata["liq_fade_out_start_next"])
    else
      0.
    end

  fade_in_delay =
    if
      list.assoc.mem("liq_fade_in_delay", initial_fade_in_metadata)
    then
      float_of_string(initial_fade_in_metadata["liq_fade_in_delay"])
    else
      fade_out_start_next
    end

  fade_in_delay =
    if
      source.duration(b) < source.duration(a)
    then
      delay = source.duration(a) - source.duration(b)
      log(
        "Adding #{delay} of fade_in_delay to match ending source"
      )
      fade_in_delay + delay
    else
      fade_in_delay
    end

  def fade.out(s) =
    fade.out(
      type="sin",
      persist_overrides=true,
      duration=fade_out,
      initial_metadata=initial_fade_out_metadata,
      s
    )
  end

  def fade.in(s) =
    fade.in(
      type="sin",
      persist_overrides=true,
      duration=fade_in,
      initial_metadata=[
        ...list.assoc.remove("liq_fade_in_delay", initial_fade_in_metadata),
        ("liq_fade_in_delay", "#{fade_in_delay}")
      ],
      s
    )
  end

  add = fun (a, b) -> add(normalize=false, [starting_map(b), ending_map(a)])
  add(fade.out(a), fade.in(b))
end

# @docof cross
# @param ~deduplicate  Crossfade transitions can generate duplicate metadata. When `true`, the operator \
#                      removes duplicate metadata from the returned source.
def replaces cross(%argsof(cross), ~deduplicate=true, transition, s) =
  if
    not deduplicate
  then
    cross(%argsof(cross), transition, s)
  else
    s = cross(%argsof(cross[!id]), transition, s)
    dedup = metadata.deduplicate(s)

    dedup.{
      buffered=s.buffered,
      duration=s.duration,
      cross_duration=s.cross_duration
    }
  end
end

# Crossfade between tracks, taking the respective volume levels into account in
# the choice of the transition.
# @category Source / Fade
# @argsof cross[id,duration,override_duration,persist_override,width]
# @param ~deduplicate  Crossfade transitions can generate duplicate metadata. When `true`, the operator \
#                      removes duplicate metadata from the returned source.
# @param s             The input source.
def crossfade(
  %argsof(cross[id,duration,override_duration,persist_override,width]),
  ~fade_in=3.,
  ~fade_out=3.,
  ~deduplicate=true,
  s
) =
  id = string.id.default(default="crossfade", id)

  def log(~level=3, x) =
    log(label=id, level=level, x)
  end

  def transition(a, b) =
    list.iter(
      fun (x) ->
        log(
          level=4,
          "Before: #{x}"
        ),
      a.metadata
    )

    list.iter(
      fun (x) ->
        log(
          level=4,
          "After : #{x}"
        ),
      b.metadata
    )

    cross.simple(
      log=log,
      fade_in=fade_in,
      fade_out=fade_out,
      initial_fade_in_metadata=b.metadata,
      initial_fade_out_metadata=a.metadata,
      a.source,
      b.source
    )
  end

  cross(
    %argsof(cross[id,duration,override_duration,persist_override,width]),
    deduplicate=deduplicate,
    transition,
    s
  )
end
