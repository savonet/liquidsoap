# Samplerate for audio.
# @category Settings
def audio.samplerate =
  settings.frame.audio.samplerate
end

# Channels for audio.
# @category Settings
def audio.channels =
  settings.frame.audio.channels
end

# Multiply the amplitude of the signal.
# @category Source / Audio processing
# @param f Multiplicative factor.
# @argsof audio.amplify
def amplify(%argsof(audio.amplify), f, s) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=audio.amplify(%argsof(audio.amplify), f, tracks.audio)})
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
# @category Source / Audio processing
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
# @category Source / Audio processing
def limit(~attack=getter(50.), ~release=getter(200.), ~ratio=getter(20.), ~threshold=getter(-2.), ~pre_gain=getter(0.), ~gain=getter(0.), s)
  compress(attack=attack, release=release, ratio=ratio, threshold=threshold, pre_gain=pre_gain, gain=gain, s)
end

# A bandpass filter obtained by chaining a low-pass and a high-pass filter.
# @category Source / Audio processing
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
# @category Source / Audio processing
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
# @category Source / Audio processing
# @flag extra
# @param s The input source.
def nrj(s)
  compress(threshold=-15.,ratio=3.,gain=3.,normalize(s))
end

# Multiband-compression.
# @category Source / Audio processing
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
# @category Source / Audio processing
# @param ~frequency Frequency below which sound is considered as bass.
# @param ~gain Amount of boosting (dB).
# @param s Source whose bass should be boosted
def bass_boost(~frequency=getter(200.), ~gain=getter(10.), s)
  bass = limit(pre_gain=gain, filter.iir.eq.low(frequency=frequency, s))
  add([s, bass])
end

%ifdef soundtouch
# Increases the pitch, making voices sound like on helium.
# @category Source / Audio processing
# @flag extra
# @param s The input source.
def helium(s)
  soundtouch(pitch=1.5,s)
end
%endif

# Remove low frequencies often produced by microphones.
# @category Source / Audio processing
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
# @category Source / Audio processing
# @param ~power If `true` use constant power mixing.
# @param wetness Wetness coefficient, from 0 (fully dry) to  1 (fully wet).
# @param dry Dry source.
# @param wet Wet source.
def dry_wet(~power=false, wetness, dry, wet)
  add(power=power, weights=[getter.map(fun(x) -> 1.-x, wetness), wetness], [dry, wet])
end

# Generate DTMF tones.
# @category Source / Sound synthesis
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
# @category Source / Audio processing
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
  let {metadata=_, ...tracks} = source.tracks(s)
  s = source(tracks)
  s = insert_metadata(s)
  insert_metadata_fn := s.insert_metadata
  let {track_marks=_, ...tracks} = source.tracks(s)
  s = source(id=id, tracks)

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
                           let [i, v] = r/\s/.split(v)
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
                           let [i, b] = r/\s/.split(arg)
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
                           let [i, b] = r/\s/.split(arg)
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

# Swap two channels of a stereo source.
# @category Source / Conversion
def swap(id=null(), (s:source))
  tracks = source.tracks(s)
  source(id=id, tracks.{audio = audio.swap(tracks.audio)})
end

# Produce mono audio by taking the mean of all audio channels.
# @category Source / Conversion
# @argsof audio.mean
def mean(%argsof(audio.mean), (s:source))
  tracks = source.tracks(s)
  source(id=id, tracks.{audio = audio.mean(%argsof(audio.mean), tracks.audio)})
end

# Convert any pcm audio source into a stereo source.
# @category Source / Conversion
def stereo(~id=null("stereo"), (s:source)) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=audio.stereo(tracks.audio)})
end

# Extract the left channel of a stereo track
# @category Source / Conversion
# @param t Track to extract from
def audio.stereo.left(~id=null("audio.stereo.left"), t)
  audio.mean(id=id, audio.stereo.pan(pan=-1., t))
end

# Extract the left channel of a stereo source
# @category Source / Conversion
# @param s Source to extract from
def stereo.left(~id=null("stereo.left"), (s:source))
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=audio.stereo.left(tracks.audio)})
end

# Extract the right channel of a stereo track
# @category Source / Conversion
# @param s Track to extract from
def audio.stereo.right(~id=null("audio.stereo.right"), t)
  audio.mean(id=id, audio.stereo.pan(pan=1., t))
end

# Extract the right channel of a stereo source
# @category Source / Conversion
# @param s Source to extract from
def stereo.right(~id=null("stereo.right"), (s:source))
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=audio.stereo.right(tracks.audio)})
end

# Spacializer which allows controlling the width of the signal.
# @category Source / Audio processing
# @param w Width of the signal (-1: mono, 0.: original, 1.: wide stereo).
def stereo.width(~id=null("stereo.width"), w=getter(0.), (s:source)) =
   tracks = source.tracks(s)
   source(id=id, tracks.{audio=audio.stereo.width(w, tracks.audio)})
end

# Pan a stereo sound.
# @category Source / Audio processing
# @argsof audio.stereo.pan
def stereo.pan(%argsof(audio.stereo.pan), (s:source)) =
   tracks = source.tracks(s)
   source(id=id, tracks.{audio=audio.stereo.pan(%argsof(audio.stereo.pan), tracks.audio)})
end

# Slow down or accelerate an audio stream by stretching (sounds lower) or squeezing it (sounds higher).
# @category Source / Audio processing
# @argsof audio.stretch
def stretch(%argsof(audio.stretch), (s:source)) =
  tracks = source.tracks(s)
  source.audio(id=id, audio.stretch(%argsof(audio.stretch), tracks.audio))
end

let stereo.ms = ()

# Decode mid+side stereo (M/S) to left+right stereo.
# @category Source / Audio processing
# @argsof audio.stereo.ms.decode
def stereo.ms.decode(%argsof(audio.stereo.ms.decode), (s:source)) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=audio.stereo.ms.decode(%argsof(audio.stereo.ms.decode), tracks.audio)})
end

# Encode left+right stereo to mid+side stereo (M/S).
# @category Source / Audio processing
# @argsof audio.stereo.ms.encode
def stereo.ms.encode(%argsof(audio.stereo.ms.encode), (s:source)) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=audio.stereo.ms.encode(%argsof(audio.stereo.ms.encode), tracks.audio)})
end

# Indicate beats.
# @category Source / Sound synthesis
# @param ~frequency Frequency of the sound.
# @param bpm Number of beats per minute.
# @flag extra
def metronome(~frequency=440., bpm=60.)
  volume_down = 0.
  beat_duration = 0.1
  s = sine(frequency)
  def f() =
    if s.time() mod (60. / bpm) <= beat_duration then
      1.
    else
      volume_down
    end
  end

  amplify(f,s)
end
