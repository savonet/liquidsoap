# Normalization the volume of a stream (this is also called _automatic gain
# control_). Dynamic normalization of the signal is sometimes the only option
# (for instance, for live sources), and can make a listening experience much
# nicer. However, its dynamic aspect implies some limitations which can go as
# far as creating saturation in some extreme cases. If possible, consider using
# some track-based normalization techniques such as those based on
# ReplayGain. The implementation of Liquidsoap < 2.0 was renamed to
# `normalize.old`.
# @category Source / Sound Processing
# @param ~id Force the value of the source ID.
# @param ~gain_max Maximal gain value (dB).
# @param ~gain_min Minimal gain value (dB).
# @param ~down Characteristic time to go down.
# @param ~up Characteristic time to go up.
# @param ~lookahead How much time to look ahead of the signal (second). Setting a positive value delays the output by the corresponding amount of time.
# @param ~target Desired RMS (dB).
# @param ~threshold Minimal RMS for activaing gain control (dB).
# @param ~window Duration of the window used to compute the current RMS power (second).
# @param ~debug How often to print debug messages, in seconds, useful to finetune the parameters. You should set `set("log.level", 5)` to see them.
# @param s Source to normalize.
# @method gain Current amplification coefficient (in linear scale).
# @method target_gain Current target amplification coefficient (in linear scale).
# @method rms Current rms (in linear scale).
def replaces normalize(~id="", ~target=getter(-13.), ~up=getter(10.), ~down=getter(.1), ~gain_min=-12., ~gain_max=12., ~lookahead=getter(0.), ~window=getter(.5), ~threshold=getter(-40.), ~track_sensitive=true, ~debug=null(), s)
  s = rms.smooth(duration=window, s)
  v = ref(1.)
  frame = frame.duration()
  gain_min = lin_of_dB(gain_min)
  gain_max = lin_of_dB(gain_max)
  def update()
    target    = lin_of_dB(getter.get(target))
    threshold = lin_of_dB(getter.get(threshold))
    rms       = s.rms()
    if rms >= threshold then
      if !v * rms <= target then
        up = 1. - exp (0. - frame / getter.get(up))
        v := !v + up * ((target / rms) - !v)
      else
        down = 1. - exp (0. - frame / getter.get(down))
        v := !v + down * ((target / rms) - !v)
      end
      v := max(gain_min, min(gain_max, !v))
    end
  end
  rms = s.rms
  def target_gain () = lin_of_dB(getter.get(target)) / rms() end
  s =
    if null.defined(debug) then
      source.run(s, every=null.get(debug), {log.debug("rms: #{s.rms()} / #{lin_of_dB(getter.get(target))}\tgain: #{!v} / #{target_gain()}")})
    else
      (s:source)
    end
  s = source.on_frame(s, update)
  if track_sensitive then source.on_track(s, fun (_) -> v := 1.) end
  amplify({!v}, delay_line(lookahead, s)).{ rms = rms, gain = fun() -> !v, target_gain = target_gain }
end


# A limiter. This is a `compress` with tweaked parameters.
# @category Source / Sound Processing
def limit(~attack=getter(50.), ~release=getter(200.), ~ratio=getter(20.), ~threshold=getter(-2.), ~pre_gain=getter(0.), ~gain=getter(0.), s)
  compress(attack=attack, release=release, ratio=ratio, threshold=threshold, pre_gain=pre_gain, gain=gain, s)
end

let limiter = limit

# Multiband compression. The list in argument specifies
# - the `frequency` below which we should apply compression (it is above previous band)
# - the `attack` time (ms)
# - the `release` time (ms)
# - the compression `ratio`
# - the `threshold` for compression
# - the `gain` for the band
# @category Source / Sound Processing
# @param ~limit Also apply limiting to bands.
# @param l Parameters for compression bands.
# @param s Source on which multiband compression should be applied.
def compress.multiband(~limit=true, ~wet=getter(1.), s, l)
  # Check that the bands are with increasing frequencies.
  for i = 0 to list.length(l) - 2 do
    assert (list.nth(l, i+1).frequency > list.nth(l, i).frequency)
  end
  # Process a band
  def band(low, band)
    s = if low > 0. then filter.iir.eq.high(frequency=low, s) else s end
    s = if band.frequency < 20000. then filter.iir.eq.low(frequency=band.frequency, s) else s end
    s = compress(attack=band.attack, release=band.release, threshold=band.threshold, ratio=band.ratio, gain=band.gain, s)
    if limit then limiter(s) else s end
  end
  l = list.mapi(fun (i, b) -> band(if i == 0 then 0. else list.nth(l, i-1).frequency end, b), l)
  c = add(normalize=false, l)
  s =
    if not getter.is_constant(wet) or getter.get(wet) != 1. then
      add(normalize=false, [amplify({1.-getter.get(wet)}, s), amplify(wet, c)])
    else
      c
    end
  # Limit to avoid bad surprises
  limiter(s)
end

# Compress and normalize, producing a more uniform and "full" sound.
# @category Source / Sound Processing
# @flag extra
# @param s The input source.
def nrj(s)
  compress(threshold=-15.,ratio=3.,gain=3.,normalize(s))
end

# Multiband-compression.
# @category Source / Sound Processing
# @flag extra
# @param s The input source.
def sky(s)
  # 3-band crossover
  low = filter.iir.eq.low(frequency = 168.)
  mh = filter.iir.eq.high(frequency = 100.)
  mid = filter.iir.eq.low(frequency = 1800.)
  high = filter.iir.eq.high(frequency = 1366.)

  # Add back
  add(normalize = false,
      [ compress(attack = 100., release = 200., threshold = -20.,
                 ratio = 6., gain = 6.7, knee = 0.3,
                 low(s)),
        compress(attack = 100., release = 200., threshold = -20.,
                 ratio = 6., gain = 6.7, knee = 0.3,
                 mid(mh(s))),
        compress(attack = 100., release = 200., threshold = -20.,
                 ratio = 6., gain = 6.7, knee = 0.3,
                 high(s))
      ])
end

# Add some bass to the sound.
# @category Source / Sound Processing
# @param ~frequency Frequency below which sound is considered as bass.
# @param ~gain Amount of boosting (dB).
# @param s Source whose bass should be boosted
def bass_boost(~frequency=200., ~gain=getter(10.), s)
  bass = limit(pre_gain=gain, filter.iir.eq.low(frequency=frequency, s))
  add([s, bass])
end

# Amplify source tracks according to replaygain metadata. This operator does not
# compute itself the replaygain: you can use either `enable_replaygain_metadata`
# or the `replaygain:` protocol for this.
# @category Source / Sound Processing
# @param s Source to be amplified.
def replaygain(s)
  amplify(override="replaygain_track_gain", 1., s)
end

%ifdef soundtouch
# Increases the pitch, making voices sound like on helium.
# @category Source / Sound Processing
# @flag extra
# @param s The input source.
def helium(s)
  soundtouch(pitch=1.5,s)
end
%endif

# Remove low frequencies often produced by microphones.
# @category Source / Sound Processing
# @param s The input source.
def mic_filter(s)
  filter(freq=200.,q=1.,mode="high",s)
end
