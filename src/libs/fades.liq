let settings.crossfade.reconcile_duration = settings.make(
  description="Default value for `reconcile_duration`", false)

fade = ()

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
  ~curve=null(),
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

  exp_curve = curve ?? 2.
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
      t <= 0.
    then
      if increasing_fade then 0. else 1. end
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
  amplify(id=id, override=null(), x, s)
end

# Fade the end of tracks.
# @category Source / Fade
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override. \
#                  Defaults to `settings.crossfade.fade_out_duration` if `null`.
# @param ~delay Initial delay before starting fade.
# @param ~curve Fade curve. Default if `null`.
# @param ~override_duration Metadata field which, if present and containing a float, overrides the 'duration' parameter for the current track.
# @param ~override_type Metadata field which, if present and correct, overrides the 'type' parameter for the current track.
# @param ~override_curve Metadata field which, if presents and correct, overrides the `curve` parameter for the current track. Use `"default"` \
#                        to set to default value.
# @param ~override_delay Metadata field which, if presents and correct, overrides the initial fade delay.
# @param ~persist_overrides Keep duration and type overrides on track change.
# @param ~track_sensitive Be track sensitive (if `false` we only fade ou once at the beginning of the track).
# @param ~initial_metadata Initial metadata.
# @param ~type Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
def fade.out(
  ~id="fade.out",
  ~duration=null(),
  ~delay=0.,
  ~curve=null(),
  ~override_duration="liq_fade_out",
  ~override_type="liq_fade_out_type",
  ~override_curve="liq_fade_out_curve",
  ~override_delay="liq_fade_out_delay",
  ~persist_overrides=false,
  ~track_sensitive=false,
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
  duration = duration ?? settings.crossfade.fade_out_duration()
  original_duration = duration
  duration = ref(duration)
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
        if d < duration() then (0., d) else (d - duration(), duration()) end
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
        curve := null()
      else
        try
          curve := float_of_string(m[override_curve])
        catch _ do
          curve := null()
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
        "Setting fade out to default duration: #{original_duration}s, delay: \
        #{original_delay}s, type: #{original_type}, curve: #{original_curve}"
      )
      duration := original_duration
      delay := original_delay
      type := original_type
      curve := original_curve
    end
  end

  s = source.on_track(s, reset_overrides)
  s = source.on_metadata(s, update_fade)
  s = if track_sensitive then source.on_track(s, stop_fade) else s end

  s =
    if
      track_sensitive
    then
      source.on_end(s, delay=duration, start_fade)
    else
      source.on_frame(s, memoize({start_fade(source.remaining(s), [])}))
    end
  fade.scale(id=id, apply, s).{fade_duration={duration()}, fade_type={type()}}
end

# Fade when the metadata trigger is received and then skip.
# @category Source / Fade
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
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
  ~curve=null(),
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
        curve := null()
      else
        try
          curve := float_of_string(m[override_curve])
        catch _ do
          curve := null()
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
        "Setting fade skip to default duration: #{original_duration}s, \
        type: #{original_type}, curve: #{original_curve}"
      )
      duration := original_duration
      type := original_type
      curve := original_curve
    end
  end

  s = source.on_track(s, reset_overrides)
  s = source.on_metadata(s, update_fade)
  s = source.on_track(s, stop_fade)
  fade.scale(id=id, apply, s).{
    fade_duration={duration()},
    fade_type={type()}
  }
end

# Fade the beginning of tracks.
# @category Source / Fade
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.\
#                  Defaults to `settings.crossfade.fade_in_duration` if `null`.
# @param ~delay Initial delay before starting fade.
# @param ~curve Fade curve. Default if `null`.
# @param ~override_duration Metadata field which, if present and containing a float, overrides the 'duration' parameter for the current track.
# @param ~override_type Metadata field which, if present and correct, overrides the 'type' parameter for the current track.
# @param ~override_curve Metadata field which, if presents and correct, overrides the `curve` parameter for the current track. Use `"default"` \
#                        to set to default value.
# @param ~override_delay Metadata field which, if presents and correct, overrides the initial fade delay.
# @param ~persist_overrides Keep duration and type overrides on track change.
# @param ~track_sensitive Be track sensitive (if `false` we only fade in once at the beginning of the track).
# @param ~initial_metadata Initial metadata.
# @param ~type Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
def fade.in(
  ~id="fade.in",
  ~duration=null(),
  ~delay=0.,
  ~curve=null(),
  ~override_duration="liq_fade_in",
  ~override_type="liq_fade_in_type",
  ~override_curve="liq_fade_in_curve",
  ~override_delay="liq_fade_in_delay",
  ~persist_overrides=false,
  ~track_sensitive=false,
  ~initial_metadata=[],
  ~type="lin",
  s
) =
  def log(x) =
    log(label=source.id(s), level=4, x)
  end

  fn = ref(fun () -> 0.)
  duration = duration ?? settings.crossfade.fade_in_duration()
  original_duration = duration
  duration = ref(duration)
  original_delay = delay
  delay = ref(original_delay)
  original_type = type
  type = ref(type)
  original_curve = curve
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
       if source.remaining(s) < duration() then
         source.remaining(s)
       else
         duration()
        end

    log(
      "Fading in with type: #{type()}, curve: #{curve_log}, delay: #{delay()}s \
       and duration: #{duration}s."
    )
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
        curve := null()
      else
        try
          curve := float_of_string(m[override_curve])
        catch _ do
          curve := null()
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
        "Setting fade in to default duration: #{original_duration}s, \
        delay: #{original_delay}s, type: #{original_type}, curve: #{original_curve}"
      )
      duration := original_duration
      delay := original_delay
      type := original_type
      curve := original_curve
    end
  end

  s = source.on_track(s, reset_overrides)
  s = source.on_metadata(s, update_fade)
  s = sequence(merge=true, [blank(duration=delay), s])
  s = fade.scale(apply, s)

  s =
    if
      track_sensitive
    then
      source.on_track(id=id, s, start_fade)
    else
      source.on_frame(id=id, s, memoize({start_fade([])}))
    end
  s.{fade_duration={duration()}, fade_delay={delay()}, fade_type={type()}}
end

# Simple transition for crossfade
# @category Source / Fade
# @param ~fade_in  Fade-in duration, if any.
# @param ~fade_out Fade-out duration, if any.
# @param ~initial_fade_in_metadata Initial fade-in metadata
# @param ~initial_fade_out_metadata Initial fade-out metadata
# @param a Ending track
# @param b Starting track
def cross.simple(
  ~fade_in=3.,
  ~fade_out=3.,
  ~initial_fade_in_metadata=[],
  ~initial_fade_out_metadata=[],
  a,
  b
) =
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
      initial_metadata=initial_fade_in_metadata,
      s
    )
  end
  add = fun (a, b) -> add(normalize=false, [b, a])

  add(fade.out(a), fade.in(b))
end

# Smart transition for crossfade
# @category Source / Fade
# @param ~log Default logger
# @param ~fade_in  Fade-in duration, if any.
# @param ~fade_out Fade-out duration, if any.
# @param ~high     Value, in dB, for loud sound level.
# @param ~medium   Value, in dB, for medium sound level.
# @param ~margin   Margin to detect sources that have too different sound level for crossing.
# @param ~default Smart crossfade: transition used when no rule applies (default: sequence).
# @param a Ending track
# @param b Starting track
def cross.smart(
  ~log=fun (x) -> log(label="cross.smart", x),
  ~fade_in=3.,
  ~fade_out=3.,
  ~default=(fun (a, b) -> (sequence([a, b]) : source)),
  ~high=-15.,
  ~medium=-32.,
  ~margin=4.,
  a,
  b
) =
  def fade.out(s) =
    fade.out(
      persist_overrides=true,
      type="sin",
      duration=fade_out,
      initial_metadata=s.metadata,
      s.source
    )
  end
  def fade.in(s) =
    fade.in(
      persist_overrides=true,
      type="sin",
      duration=fade_in,
      initial_metadata=s.metadata,
      s.source
    )
  end
  add = fun (a, b) -> add(normalize=false, [b, a])

  if
    # If A and B are not too loud and close, fully cross-fade them.
    a.db_level <= medium
  and
    b.db_level <= medium
  and
    abs(a.db_level - b.db_level) <= margin
  then
    log(
      "Old <= medium, new <= medium and |old-new| <= margin."
    )
    log(
      "Old and new source are not too loud and close."
    )
    log(
      "Transition: crossed, fade-in, fade-out."
    )
    add(fade.out(a), fade.in(b))
  elsif
    # If B is significantly louder than A, only fade-out A.
    # We don't want to fade almost silent things, ask for >medium.
    b.db_level >= a.db_level + margin
  and
    a.db_level >= medium
  and
    b.db_level <= high
  then
    log(
      "new >= old + margin, old >= medium and new <= high."
    )
    log(
      "New source is significantly louder than old one."
    )
    log(
      "Transition: crossed, fade-out."
    )
    add(fade.out(a), b.source)
  elsif
    # Opposite as the previous one.
    a.db_level >= b.db_level + margin
  and
    b.db_level >= medium
  and
    a.db_level <= high
  then
    log(
      "old >= new + margin, new >= medium and old <= high"
    )
    log(
      "Old source is significantly louder than new one."
    )
    log(
      "Transition: crossed, fade-in."
    )
    add(a.source, fade.in(b))
  elsif
    # Do not fade if it's already very low.
    b.db_level >= a.db_level + margin
  and
    a.db_level <= medium
  and
    b.db_level <= high
  then
    log(
      "new >= old + margin, old <= medium and new <= high."
    )
    log(
      "Do not fade if it's already very low."
    )
    log(
      "Transition: crossed, no fade."
    )
    add(a.source, b.source)

    # What to do with a loud end and a quiet beginning ?
    # A good idea is to use a jingle to separate the two tracks,
    # but that's another story.
  else
    # Otherwise, A and B are just too loud to overlap nicely, or the
    # difference between them is too large and overlapping would completely
    # mask one of them.
    log(
      "No transition: using default."
    )
    default(a.source, b.source)
  end
end



# Crossfade between tracks, taking the respective volume levels into account in
# the choice of the transition.
# @category Source / Fade
# @param ~id           Force the value of the source ID.
# @param ~duration     Duration (in seconds) of buffered data from each track \
#                      that is used to compute the transition between tracks.
# @param ~override_duration \
#                      Metadata field which, if present and containing a \
#                      float, overrides the 'duration' parameter for current \
#                      track.
# @param ~persist_override Keep duration override on track change.
# @param ~fade_in      Fade-in duration, if any.
# @param ~fade_out     Fade-out duration, if any.
# @param ~width        Width of the volume analysis window.
# @param ~conservative Always prepare for a premature end-of-track.
# @param ~minimum      Minimum duration (in sec.) for a cross: \
#                      If the track ends without any warning (e.g. in case of skip) \
#                      there may not be enough data for a decent composition. \
#                      Set to 0. to avoid having transitions after skips, \
#                      or more to avoid transitions on short tracks. \
#                      With a negative default, transitions always occur.
# @param ~default      Smart crossfade: transition used when no rule applies \
#                      (default: sequence).
# @param ~reconcile_duration \
#                      When set to `true`, if duration for the new track is shorter than the ending \
#                      duration, a `liq_fade_in_delay` metadata is automatically injected to make \
#                      crossfades match. Defaults to `settings.crossfade.reconcile_duration` if `null`.
# @param ~override_fade_in_delay \
#                      Metadata field used to inject metadata overriding fade-in delay \
#                      when reonciling crossfade duration.
# @param ~smart        Enable smart crossfading
# @param ~high         Smart crossfade: value, in dB, for loud sound level.
# @param ~medium       Smart crossfade: value, in dB, for medium sound level.
# @param ~margin       Smart crossfade: margin to detect sources that have too different \
#                      sound level for crossing.
# @param ~deduplicate  Crossfade transitions can generate duplicate metadata. When `true`, the operator \
#                      removes duplicate metadata from the returned source.
# @param s             The input source.
def crossfade(
  ~id=null(),
  ~duration=5.,
  ~override_duration="liq_cross_duration",
  ~reconcile_duration=null(),
  ~override_fade_in_delay="liq_fade_in_delay",
  ~persist_override=false,
  ~fade_in=3.,
  ~fade_out=3.,
  ~smart=false,
  ~default=(fun (a, b) -> (sequence([a, b]) : source)),
  ~high=-15.,
  ~medium=-32.,
  ~margin=4.,
  ~deduplicate=true,
  ~minimum=(-1.),
  ~width=2.,
  ~conservative=true,
  s
) =
  id = string.id.default(default="crossfade", id)
  reconcile_duration = reconcile_duration ?? settings.crossfade.reconcile_duration()

  def log(~level=3, x) =
    log(label=id, level=level, x)
  end

  def simple_transition(a, b) =
    list.iter(
      fun (x) ->
        log(
          level=4,
          "Before: #{x}"
        ),
      metadata.cover.remove(a.metadata)
    )
    list.iter(
      fun (x) ->
        log(
          level=4,
          "After : #{x}"
        ),
      metadata.cover.remove(b.metadata)
    )

    log(
      "Simple transition: crossed, fade-in, fade-out."
    )
    cross.simple(
      fade_in=fade_in,
      fade_out=fade_out,
      initial_fade_in_metadata=b.metadata,
      initial_fade_out_metadata=a.metadata,
      a.source,
      b.source
    )
  end

  def smart_transition(a, b) =
    list.iter(
      fun (x) ->
        log(
          level=4,
          "Before: #{x}"
        ),
      metadata.cover.remove(a.metadata)
    )
    list.iter(
      fun (x) ->
        log(
          level=4,
          "After : #{x}"
        ),
      metadata.cover.remove(b.metadata)
    )

    cross.smart(
      log=log,
      fade_in=fade_in,
      fade_out=fade_out,
      default=default,
      high=high,
      medium=medium,
      margin=margin,
      a,
      b
    )
  end

  transition = if smart then smart_transition else simple_transition end

  let (cross_id, deduplicate_id) =
    deduplicate ? (null(), null(id)) : (null(id), null())

  crossed =
    cross(
      id=cross_id,
      width=width,
      duration=duration,
      persist_override=persist_override,
      reconcile_duration=reconcile_duration,
      override_fade_in_delay=override_fade_in_delay,
      override_duration=override_duration,
      conservative=conservative,
      minimum=minimum,
      transition,
      s
    )

  s =
    if
      deduplicate
    then
      metadata.deduplicate(id=deduplicate_id, crossed)
    else
      crossed
    end

  s.{cross_duration={crossed.cross_duration()}}
end
