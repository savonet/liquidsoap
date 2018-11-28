# Make a fade function based on a source's clock.
# @category Math
# @param ~type Fade shape. One of: "sin", "exp", "log", "lin"
# @param ~start Start value.
# @param ~stop Stop value.
# @param ~duration Duration in seconds.
# @param s Source clock
def mkfade(~type="lin",~start=0.,~stop=1.,~duration=3.,s) =
  log = log(label="mkfade")

  # Shape functions must map 0. -> 0. and 1. -> 1.
  pi = acos(-1.)
  def sin_shape(x) =
    (1. + sin((x-0.5)*pi))/2.
  end

  curve = 2.
  m = exp(curve-1.) - exp(-1.)
  def exp_shape(x) =
    (exp((curve*x)-1.) - exp(-1.))/m
  end

  curve = 10.
  m = log_e(1.+curve)
  def log_shape(x) =
    log_e(1.+10.*x)/m
  end

  def lin_shape(x) =
    x
  end

  shape =
    if type == "sin" then
      sin_shape
    elsif type == "exp" then
      exp_shape
    elsif type == "log" then
      log_shape
    elsif type == "lin" then
      lin_shape
    else 
      log("Invalid type #{type}, using \"lin\"")
      lin_shape
    end

  start_time = source.time(s)
  def fade() =
    t = source.time(s) - start_time
    if t >= duration then
      stop
    else
      if start <= stop then
        start + shape(t/duration)*(stop-start)
      else
        stop + shape(1.-t/duration)*(start-stop)
      end
    end
  end

  fade
end

# flag @hidden
def fade.out.base(~id,~duration,~override_duration,
                  ~override_type,~type,~final,s) =
  log = log(label=source.id(s),level=4)
  fn = ref (fun () -> 1.)
  type = ref type
  duration = ref duration

  def apply() =
    fn = !fn
    fn()
  end

  def start_fade(d,_) =
    log("Fading out with #{d}s remaining..")
    duration = if d < !duration then d else !duration end
    fn := mkfade(start=1.,stop=0.,type=!type,duration=duration,s)
  end

  def stop_fade(_) =
    if final then
      fn := fun () -> 0.
    else
      fn := fun () -> 1.
    end
  end

  def update_fade(m) =
    if m[override_duration] != "" then
      duration := float_of_string(default=!duration,m[override_duration])
    end

    if m[override_type] != "" then
      type := m[override_type]
    end
  end

  delay = fun () -> !duration
  s = on_end(delay=delay,start_fade,s)
  s = on_track(stop_fade,s)
  s = on_metadata(update_fade,s)
  s = amplify(id=id,apply,s)

  if final then
    start_fade(!duration,())
    sequence([s,sequence([])])
  else
    s
  end
end

# Fade the end of tracks.
# @category Source / Sound Processing
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
# @param ~override_duration Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
# @param ~override_type Metadata field which, if present and correct, overrides the 'type' parameter for current track.
# @param ~type Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
def fade.out(~id="fade.out",~duration=3.,
             ~override_duration="liq_fade_out",
             ~override_type="liq_fade_type",
             ~type="lin",s) =
  fade.out.base(id=id,duration=duration,
                override_duration=override_duration,
                override_type=override_type,
                type=type,final=false,s)
end

# Fade a stream to silence.
# @category Source / Sound Processing
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
# @param ~override_duration Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
# @param ~override_type Metadata field which, if present and correct, overrides the 'type' parameter for current track.
# @param ~type Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
def fade.final(~id="fade.final",~duration=3.,
             ~override_duration="liq_fade_out",
             ~override_type="liq_fade_type",
             ~type="lin",s) =
  fade.out.base(id=id,duration=duration,
                override_duration=override_duration,
                override_type=override_type,
                type=type,final=true,s)
end

# @flag hidden
def fade.in.base(~id,~duration,~override_duration,~override_type,
                 ~type,~initial,s) =
  log = log(label=source.id(s),level=4)
  fn = ref (fun () -> 0.)
  duration = ref duration
  type = ref type
  started = ref false

  def apply() =
    fn = !fn
    fn()
  end

  def start_fade(_) =
    if not (initial and !started) then
      log("Fading in..")
      fn := mkfade(start=0.,stop=1.,type=!type,duration=!duration,s)
      started := true
    end
  end

  def update_fade(m) =
    if m[override_duration] != "" then
      duration := float_of_string(default=!duration,m[override_duration])
      log("New fade duration: #{!duration}")
    end

    if m[override_type] != "" then
      type := m[override_type]
      log("New fade type: #{type}")
    end
  end

  s = on_track(start_fade,s)
  s = on_metadata(update_fade,s)

  amplify(id=id,apply,s)
end

# Fade the beginning of tracks.
# @category Source / Sound Processing
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
# @param ~override_duration Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
# @param ~override_type Metadata field which, if present and correct, overrides the 'type' parameter for current track.
# @param ~type Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
def fade.in(~id="fade.in",~duration=3.,
            ~override_duration="liq_fade_in",
            ~override_type="liq_fade_type",
            ~type="lin",s) =
  fade.in.base(id=id,duration=duration,
               override_duration=override_duration,
               override_type=override_type,
               type=type,initial=false,s)
end

# Fade the beginning of a stream.
# @category Source / Sound Processing
# @param ~id Force the value of the source ID.
# @param ~duration Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
# @param ~override_duration Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
# @param ~override_type Metadata field which, if present and correct, overrides the 'type' parameter for current track.
# @param ~type Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
def fade.initial(~id="fade.in",~duration=3.,
            ~override_duration="liq_fade_in",
            ~override_type="liq_fade_type",
            ~type="lin",s) =
  fade.in.base(id=id,duration=duration,
               override_duration=override_duration,
               override_type=override_type,
               type=type,initial=false,s)
end
