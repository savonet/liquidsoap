# Samplerate for audio.
# @category Liquidsoap
def audio.samplerate =
  settings.frame.audio.samplerate
end

# Channels for audio.
# @category Liquidsoap
def audio.channels =
  settings.frame.audio.channels
end

lufs_builtin = lufs

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
# @param ~lufs Use LUFS instead of RMS to compute intensity.
# @param ~target Desired RMS (dB).
# @param ~threshold Minimal RMS for activaing gain control (dB).
# @param ~window Duration of the window used to compute the current RMS power (second).
# @param ~debug How often to print debug messages, in seconds, useful to finetune the parameters. You should set `set("log.level", 5)` to see them.
# @param s Source to normalize.
# @method gain Current amplification coefficient (in linear scale).
# @method target_gain Current target amplification coefficient (in linear scale).
# @method rms Current rms (in linear scale).
def replaces normalize(~id=null(), ~target=getter(-13.), ~up=getter(10.), ~down=getter(.1), ~gain_min=-12., ~gain_max=12., ~lufs=false, ~lookahead=getter(0.), ~window=getter(.5), ~threshold=getter(-40.), ~track_sensitive=true, ~debug=null(), s)
  let (s, rms) =
    if lufs then
      s = lufs_builtin(id=id, window=window, s)
      (s, {lin_of_dB(s.lufs())})
    else
      s = rms.smooth(id=id, duration=window, s)
      (s, s.rms)
    end
  v = ref(1.)
  frame = frame.duration()
  gain_min = lin_of_dB(gain_min)
  gain_max = lin_of_dB(gain_max)
  def update()
    target    = lin_of_dB(getter.get(target))
    threshold = lin_of_dB(getter.get(threshold))
    rms       = rms()
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
  def target_gain () = lin_of_dB(getter.get(target)) / rms() end
  s =
    if null.defined(debug) then
      source.run(s, every=null.get(debug), {log.debug("rms: #{rms()} / #{lin_of_dB(getter.get(target))}\tgain: #{!v} / #{target_gain()}")})
    else
      s
    end
  s = source.on_frame(s, update)
  s = if track_sensitive then source.on_track(s, fun (_) -> v := 1.) else s end
  amplify(id=id, {!v}, delay_line(lookahead, s)).{ rms = rms, gain = fun() -> !v, target_gain = target_gain }
end


# A limiter. This is a `compress` with tweaked parameters.
# @category Source / Sound Processing
def limit(~attack=getter(50.), ~release=getter(200.), ~ratio=getter(20.), ~threshold=getter(-2.), ~pre_gain=getter(0.), ~gain=getter(0.), s)
  compress(attack=attack, release=release, ratio=ratio, threshold=threshold, pre_gain=pre_gain, gain=gain, s)
end

# A bandpass filter obtained by chaining a low-pass and a high-pass filter.
# @category Source / Sound Processing
# @param id Force the value of the source ID.
# @param ~low Lower frequency of the bandpass filter.
# @param ~high Higher frequency of the bandpass filter.
# @param ~q Q factor.
def filter.iir.eq.low_high(~id=null(), ~low, ~high, ~q=1., s)
  s = if not (getter.is_constant(high) and getter.get(high) == infinity) then filter.iir.eq.low(id=id, frequency=high, q=q, s) else s end
  s = if not (getter.is_constant(low) and getter.get(low) == 0.) then filter.iir.eq.high(id=id, frequency=low, q=q, s) else s end
  s
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
    if getter.get(list.nth(l,i+1).frequency) < getter.get(list.nth(l,i).frequency) then failwith("Bands should be sorted.") end
  end
  # Process a band
  def band(low, band)
    high = if getter.is_constant(band.frequency) and getter.get(band.frequency) >= float_of_int(audio.samplerate()) / 2. then infinity else band.frequency end
    s = filter.iir.eq.low_high(low=low, high=high, s)
    s = compress(attack=band.attack, release=band.release, threshold=band.threshold, ratio=band.ratio, gain=band.gain, s)
    if limit then limiter(s) else s end
  end
  l = list.mapi(fun (i, b) -> band(if i == 0 then 0. else list.nth(l,i-1).frequency end, b), l)
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
  low = fun(s) -> filter.iir.eq.low(frequency = 168., s)
  mh = fun(s) -> filter.iir.eq.high(frequency = 100., s)
  mid = fun(s) -> filter.iir.eq.low(frequency = 1800., s)
  high = fun(s) -> filter.iir.eq.high(frequency = 1366., s)

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
def bass_boost(~frequency=getter(200.), ~gain=getter(10.), s)
  bass = limit(pre_gain=gain, filter.iir.eq.low(frequency=frequency, s))
  add([s, bass])
end

# Amplify source tracks according to replaygain metadata. This operator does not
# compute itself the replaygain: you can use either `enable_replaygain_metadata`
# or the `replaygain:` protocol for this.
# @category Source / Sound Processing
# @param ~id Force the value of the source ID.
# @param ~ebu_r128 Also amplify according to EBU R128 tags.
# @param s Source to be amplified.
def replaygain(~id=null(), ~ebu_r128=true, s)
  s = insert_metadata(s)
  # Normalize opus gain
  def f(m)
    def f(m)
      let (k, v) = m
      if k == "r128_track_gain" then
        v = int_of_string(v)
        v = v + 5 # normalize to -18 dB as usual replaygain instead of -23 dB
        v = lin_of_dB(float(v) / 256.)
        ("replaygain_track_gain", string_of_float(v))
      else
        (k, v)
      end
    end
    if ebu_r128 then list.map(f, m) else m end
  end
  s = metadata.map(f, s)
  amplify(id=id, override="replaygain_track_gain", 1., s)
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
# @param ~frequency Frequency under which sound should be lowered.
# @param s The input source.
def mic_filter(~frequency=200., s)
  filter(freq=frequency, q=1., mode="high", s)
end

# Mix between dry and wet sources. Useful for testing effects. Typically:
# ```
# c = interactive.float("wetness", min=0., max=1., 1.)
# s = dry_wet(c, s, effect(s))
# ```
# and vary `c` to hear the difference between the source without and with
# the effect.
# @category Source / Sound Processing
# @param ~power If `true` use constant power mixing.
# @param wetness Wetness coefficient, from 0 (fully dry) to  1 (fully wet).
# @param dry Dry source.
# @param wet Wet source.
def dry_wet(~power=false, wetness, dry, wet)
  add(power=power, weights=[getter.map(fun(x) -> 1.-x, wetness), wetness], [dry, wet])
end

# Generate DTMF tones.
# @category Source / Sound Synthesis
# @param ~duration Duration of a tone (in seconds).
# @param ~delay Dealy between two successive tones (in seconds).
# @param dtmf String describing DTMF tones to generates: it should contains characters 0 to 9, A to D, or * or #.
def replaces dtmf(~duration=0.1, ~delay=0.05, dtmf)
  l = ref([])
  for i = 0 to string.length(dtmf) - 1 do
    c = string.sub(dtmf, start=i, length=1)
    let (row, col) =
      if c == "1" then
        (697., 1209.)
      elsif c == "2" then
        (697., 1336.)
      elsif c == "3" then
        (697., 1477.)
      elsif c == "A" then
        (697., 1633.)
      elsif c == "4" then
        (770., 1209.)
      elsif c == "5" then
        (770., 1336.)
      elsif c == "6" then
        (770., 1477.)
      elsif c == "B" then
        (770., 1633.)
      elsif c == "7" then
        (852., 1209.)
      elsif c == "8" then
        (852., 1336.)
      elsif c == "9" then
        (852., 1477.)
      elsif c == "C" then
        (852., 1633.)
      elsif c == "*" then
        (941., 1209.)
      elsif c == "0" then
        (941., 1336.)
      elsif c == "#" then
        (941., 1477.)
      elsif c == "D" then
        (941., 1633.)
      else
        (0., 0.)
      end
    s = add([sine(row, duration=duration), sine(col, duration=duration)])
    l := blank(duration=delay) :: !l
    l := s :: !l
  end
  l = list.rev(!l)
  sequence(l)
end

# Mixing table controllable via source methods and optional
# server/telnet commands.
# @flag extra
# @category Source / Sound Processing
# @param ~id Force the value of the source ID.
# @param ~register_server_commands Register corresponding server commands
def mix(~id=null(), ~register_server_commands=true, sources) =
  id = string.id.default(default="mixer", id)

  inputs = list.map(fun (s) ->
    begin
      volume      = ref(1.)
      is_selected = ref(false)
      is_single   = ref(false)

      {
        volume = fun () -> !volume,
        set_volume = fun (v) -> volume := v,
        selected = fun () -> !is_selected,
        set_selected = fun (b) -> is_selected := b,
        single = fun () -> !is_single,
        set_single = fun (b) -> is_single := b,
        source = s
      }
    end, sources)

  insert_metadata_fn = ref(fun (_) -> ())

  sources = list.map(fun (input) ->
    begin
      s = amplify(input.volume, input.source)
      s = source.on_track(s, fun (_) ->
         if input.single() then
           input.set_selected(false)
         end
      )
      s = source.on_metadata(s, fun (m) ->
        begin
          fn = !insert_metadata_fn
          fn(m)
        end
      )
      switch([(input.selected, s)])
    end, inputs)

  s = add(sources)
  s = drop_metadata(s)
  s = insert_metadata(s)
  insert_metadata_fn := s.insert_metadata
  s = merge_tracks(id=id, s)

  if register_server_commands then
     def status(input) =
       "ready=#{source.is_ready(input.source)} selected=#{input.selected()} single=#{input.single()} volume=#{int_of_float(input.volume() * 100.)}% remaining=#{source.remaining(input.source)}"
     end

     server.register(namespace=source.id(s), description="Skip current track on all enabled sources.",
                      "skip", fun (_) -> begin
                         list.iter(fun (input) -> if input.selected() then source.skip(input.source) end, inputs)
                         "OK"
                      end)
     server.register(namespace=source.id(s), description="Set volume for a given source.",
                     usage="volume <source nb> <vol%>", "volume", fun (v) -> begin
                         try
                           let [i, v] = string.split(separator=" ", v)
                           input = list.nth(inputs, int_of_string(i))
                           input.set_volume(float_of_string(v))
                           status(input)
                         catch _ do
                           "Usage: volume <source nb> <vol%>"
                         end
                      end)
     server.register(namespace=source.id(s), description="Enable/disable a source.",
                     usage="select <source nb> <true|false>", "select", fun (arg) -> begin
                         try
                           let [i, b] = string.split(separator=" ", arg)
                           input = list.nth(inputs, int_of_string(i))
                           input.set_selected(b == "true")
                           status(input)
                         catch _ do
                           "Usage: select <source nb> <true|false>"
                         end
                      end)
     server.register(namespace=source.id(s), description="Enable/disable automatic stop at the end of track.",
                     usage="single <source nb> <true|false>", "single", fun (arg) -> begin
                         try
                           let [i, b] = string.split(separator=" ", arg)
                           input = list.nth(inputs, int_of_string(i))
                           input.set_single(b == "true")
                           status(input)
                         catch _ do
                           "Usage: single <source nb> <true|false>"
                         end
                      end)
     server.register(namespace=source.id(s), description="Display current status.",
                     "status", fun (i) -> begin
                         try
                           status(list.nth(inputs, int_of_string(i)))
                         catch _ do
                           "Usage: status <source nb>"
                         end
                      end)
     server.register(namespace=source.id(s), description="Print the list of input sources.",
                     "inputs", fun (_) -> string.concat(separator=" ", list.map(fun (input) -> source.id(input.source), inputs)))
  end

  s.{inputs = inputs}
end
