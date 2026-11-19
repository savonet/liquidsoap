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
    elsif
      type == "exp"
    then
      exp_shape
    elsif
      type == "log"
    then
      log_shape
    elsif
      type == "lin"
    then
      lin_shape
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

    # While the delay is pending, do not touch `duration`: a zero duration
    # would divide by zero and amplify by inf.
    if
      t < 0.
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
# @param ~allow_metadata_override Let each track change the fade with its own metadata: `liq_fade_out` for the duration, `liq_fade_out_delay`, \
#                                 `liq_fade_out_type` and `liq_fade_out_curve` (`"default"` for the default curve). When `false`, \
#                                 only the arguments given here are used.
# @param ~persist_overrides Keep what a track's metadata changed for the tracks that follow. Only with `allow_metadata_override`.
# @param ~track_sensitive Be track sensitive (if `false` we only fade ou once at the beginning of the track).
# @param ~type Fader shape. One of: "lin"", "sin", "log" or "exp". Defaults to `settings.fade.out.type` if `null`.
def fade.out(
  ~id="fade.out",
  ~duration=null,
  ~delay=0.,
  ~curve=null,
  ~allow_metadata_override=false,
  ~persist_overrides=false,
  ~track_sensitive=false,
  ~type=null,
  s
) =
  override_duration = "liq_fade_out"
  override_type = "liq_fade_out_type"
  override_curve = "liq_fade_out_curve"
  override_delay = "liq_fade_out_delay"

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
  if
    allow_metadata_override
  then
    s.on_track(synchronous=true, reset_overrides)
    s.on_metadata(synchronous=true, update_fade)
  end

  if track_sensitive then s.on_track(synchronous=true, stop_fade) end

  d = source.dynamic(memoize({s}))

  if
    track_sensitive
  then
    d.on_position(
      synchronous=true,
      remaining=true,
      allow_partial=false,
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

  fade.scale(id=id, apply, d).{
    fade_duration = {duration()},
    fade_type = {type()}
  }
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

  fade.scale(id=id, apply, s).{
    fade_duration = {duration()},
    fade_type = {type()}
  }
end

# Fade the beginning of tracks.
# @category Source / Fade
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override. Defaults to `settings.fade.in.duration` if `null`.
# @param ~delay Initial delay before starting fade.
# @param ~curve Fade curve. Defaults to `settings.fade.in.curve` if `null`.
# @param ~allow_metadata_override Let each track change the fade with its own metadata: `liq_fade_in` for the duration, `liq_fade_in_delay`, \
#                                 `liq_fade_in_type` and `liq_fade_in_curve` (`"default"` for the default curve). When `false`, \
#                                 only the arguments given here are used.
# @param ~persist_overrides Keep what a track's metadata changed for the tracks that follow. Only with `allow_metadata_override`.
# @param ~track_sensitive Be track sensitive (if `false` we only fade in once at the beginning of the track).
# @param ~type Fader shape. One of: "lin"", "sin", "log" or "exp". Defaults to `settings.fade.in.type` if `null`.
def fade.in(
  ~id="fade.in",
  ~duration=null,
  ~delay=0.,
  ~curve=null,
  ~allow_metadata_override=false,
  ~persist_overrides=false,
  ~track_sensitive=false,
  ~type=null,
  s
) =
  override_duration = "liq_fade_in"
  override_type = "liq_fade_in_type"
  override_curve = "liq_fade_in_curve"
  override_delay = "liq_fade_in_delay"

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
  if
    allow_metadata_override
  then
    s.on_track(synchronous=true, reset_overrides)
    s.on_metadata(synchronous=true, update_fade)
  end

  # Register start_fade on the inner source so it fires before the amplify
  # operator processes the new track head.
  if track_sensitive then s.on_track(synchronous=true, start_fade) end

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
    not track_sensitive
  then
    s.on_frame(synchronous=true, before=false, memoize({start_fade([])}))
  end
  s.{fade_duration = {duration()}, fade_delay = {delay()}, fade_type = {type()}}
end

# Where the fades of a crossing sit, in seconds from the start of the
# transition, computed from the metadata of both tracks and from the exact
# lengths of the two sources. This is the one place where the `liq_fade_*`
# keys are read for a crossing: to change one thing in a crossfade, change one
# number of the plan and pass it to `cross.apply`.
# @category Source / Fade
# @param ~fade_in Duration of the fade-in when the starting track does not say (`liq_fade_in`).
# @param ~fade_out Duration of the fade-out when the ending track does not say (`liq_fade_out`).
# @param ending Ending track, as given to the transition function of `cross`.
# @param starting Starting track, as given to the transition function of `cross`.
def cross.plan(~fade_in=3., ~fade_out=3., ending, starting) =
  def log(x) =
    log(label="cross.plan", level=3, x)
  end

  # The float under a key, `null` when the key is missing or is not a float.
  def float_key(metadata, key) =
    if
      list.assoc.mem(key, metadata)
    then
      try
        (float_of_string(metadata[key]) : float?)
      catch _ do
        log(
          "Ignoring `#{key}`: #{metadata[key]} is not a float."
        )
        null
      end
    else
      null
    end
  end

  def string_key(~default, metadata, key) =
    list.assoc.mem(key, metadata) ? metadata[key] : default
  end

  asked_fade_out = float_key(ending.metadata, "liq_fade_out") ?? fade_out
  asked_fade_in = float_key(starting.metadata, "liq_fade_in") ?? fade_in

  # Under `cross` both lengths are exact. A source of unknown length, given
  # to the plan outside of it, is taken to be as long as its fade.
  def known(length, fade) =
    length == infinity or length < 0. ? fade : length
  end

  ending_length = known(source.remaining(ending.source), asked_fade_out)
  starting_length = known(source.remaining(starting.source), asked_fade_in)
  fade_out_duration = min(asked_fade_out, ending_length)

  # `liq_fade_out_start_next` is relative to the start of a window of
  # `liq_fade_out_delay + liq_fade_out` that ends where the track ends: moved
  # to the window we hold, it names the same place in the track.
  start_next = float_key(ending.metadata, "liq_fade_out_start_next")
  start_next =
    if
      null.defined(start_next)
    then
      fade_out_delay = float_key(ending.metadata, "liq_fade_out_delay") ?? 0.
      (
        max(
          0.,
          null.get(start_next) + ending_length - asked_fade_out - fade_out_delay
        )
      :
        float?
      )
    else
      null
    end

  start =
    float_key(starting.metadata, "liq_fade_in_delay") ?? (start_next ?? 0.)

  # A starting source shorter than the ending one starts later: both end
  # together.
  start =
    if
      starting_length < ending_length
    then
      start + ending_length - starting_length
    else
      start
    end

  {
    duration = max(ending_length, start + starting_length),
    ending =
      {
        fade_start = ending_length - fade_out_duration,
        fade_duration = fade_out_duration,
        type = string_key(default="sin", ending.metadata, "liq_fade_out_type"),
        curve = float_key(ending.metadata, "liq_fade_out_curve")
      },
    starting =
      {
        start = start,
        fade_duration = min(asked_fade_in, starting_length),
        type = string_key(default="sin", starting.metadata, "liq_fade_in_type"),
        curve = float_key(starting.metadata, "liq_fade_in_curve")
      }
  }
end

# The transition a plan describes: the ending source faded out and the
# starting source faded in with `fade.out` and `fade.in`, each where the plan
# puts it, added together. Neither fade reads metadata.
# @category Source / Fade
# @param ~ending_map Optional mapping for the faded ending track.
# @param ~starting_map Optional mapping for the faded starting track.
# @param plan Where the fades sit, as returned by `cross.plan`.
# @param ending Ending track, as given to the transition function of `cross`.
# @param starting Starting track, as given to the transition function of `cross`.
def cross.apply(
  ~ending_map=fun (s) -> s,
  ~starting_map=fun (s) -> s,
  plan,
  ending,
  starting
) =
  faded_out =
    fade.out(
      id="cross.fade_out",
      type=plan.ending.type,
      curve=plan.ending.curve,
      delay=plan.ending.fade_start,
      duration=plan.ending.fade_duration,
      ending.source
    )

  faded_in =
    fade.in(
      id="cross.fade_in",
      type=plan.starting.type,
      curve=plan.starting.curve,
      delay=plan.starting.start,
      duration=plan.starting.fade_duration,
      starting.source
    )

  # The maps take and return plain sources, whatever methods theirs carry.
  add(
    normalize=false,
    [
      (starting_map((faded_in : source)) : source),
      (ending_map((faded_out : source)) : source)
    ]
  )
end

# Simple transition for crossfade: `cross.apply` of `cross.plan`, over two
# sources and the metadata of their tracks.
# @category Source / Fade
# @param ~log Logging utility
# @param ~fade_in  Fade-in duration, if any.
# @param ~fade_out Fade-out duration, if any.
# @param ~initial_fade_in_metadata Metadata of the starting track.
# @param ~initial_fade_out_metadata Metadata of the ending track.
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
  ending = {source = a, metadata = initial_fade_out_metadata}
  starting = {source = b, metadata = initial_fade_in_metadata}
  plan = cross.plan(fade_in=fade_in, fade_out=fade_out, ending, starting)
  log(
    "Fading out #{plan.ending.fade_duration}s from #{plan.ending.fade_start}s,   \
     fading in #{plan.starting.fade_duration}s from #{plan.starting.start}s."
  )
  cross.apply(
    ending_map=ending_map,
    starting_map=starting_map,
    plan,
    ending,
    starting
  )
end

# @docof cross
# @param ~deduplicate  Crossfade transitions can generate duplicate metadata. When `true`, the operator \
#                      removes duplicate metadata from the returned source.
def replaces cross(%argsof(cross), ~deduplicate=true, transition, s) =
  s = cross(%argsof(cross), transition, s)
  if
    deduplicate
  then
    metadata.deduplicate(s).{
      buffered = s.buffered,
      duration = s.duration,
      cross_duration = s.cross_duration
    }
  else
    s
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

  def transition(ending, starting) =
    list.iter(
      fun (x) ->
        log(
          level=4,
          "Before: #{x}"
        ),
      ending.metadata
    )

    list.iter(
      fun (x) ->
        log(
          level=4,
          "After : #{x}"
        ),
      starting.metadata
    )

    plan = cross.plan(fade_in=fade_in, fade_out=fade_out, ending, starting)
    log(
      "Fading out #{plan.ending.fade_duration}s from #{
        plan.ending.fade_start
      }s, fading in #{plan.starting.fade_duration}s from #{
        plan.starting.start
      }s."
    )
    cross.apply(plan, ending, starting)
  end

  cross(
    %argsof(cross[id,duration,override_duration,persist_override,width]),
    deduplicate=deduplicate,
    transition,
    s
  )
end
