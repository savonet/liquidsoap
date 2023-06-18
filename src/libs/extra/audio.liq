# Compand the signal.
# @category Source / Audio processing
# @flag extra
# @argsof track.audio.compand
def compand(~id=null("compand"), %argsof(track.audio.compand[!id]), s) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=track.audio.compand(%argsof(track.audio.compand), tracks.audio)})
end

# Comb filter
# @category Source / Audio processing
# @argsof track.audio.comb
# @flag extra
def comb(~id=null("comb"), %argsof(track.audio.comb[!id]), s) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=track.audio.comb(%argsof(track.audio.comb), tracks.audio)})
end

# Compress the signal.
# @category Source / Audio processing
# @argsof track.audio.compress
# @flag extra
def compress(%argsof(track.audio.compress), s) =
  tracks = source.tracks(s)
  let audio.{gain, rms} = track.audio.compress(%argsof(track.audio.compress), tracks.audio)
  source(id=id, tracks.{audio=audio}).{gain=gain, rms=rms}
end

# Exponential compressor.
# @category Source / Audio processing
# @argsof track.audio.compress.exponential
# @flag extra
def compress.exponential(%argsof(track.audio.compress.exponential), s) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=track.audio.compress.exponential(%argsof(track.audio.compress.exponential), tracks.audio)})
end

# A limiter. This is a `compress` with tweaked parameters.
# @category Source / Audio processing
# @flag extra
def limit(~id=null(), ~attack=getter(50.), ~release=getter(200.), ~ratio=getter(20.), ~threshold=getter(-2.), ~pre_gain=getter(0.), ~gain=getter(0.), s)
  compress(id=id, attack=attack, release=release, ratio=ratio, threshold=threshold, pre_gain=pre_gain, gain=gain, s)
end

let limiter = limit

# A bandpass filter obtained by chaining a low-pass and a high-pass filter.
# @category Source / Audio processing
# @flag extra
# @param id Force the value of the source ID.
# @param ~low Lower frequency of the bandpass filter.
# @param ~high Higher frequency of the bandpass filter.
# @param ~q Q factor.
def filter.iir.eq.low_high(~id=null(), ~low, ~high, ~q=1., s)
  s = if not (getter.is_constant(high) and getter.get(high) == infinity) then filter.iir.eq.low(id=id, frequency=high, q=q, s) else s end
  s = if not (getter.is_constant(low) and getter.get(low) == 0.) then filter.iir.eq.high(id=id, frequency=low, q=q, s) else s end
  s
end

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
# @flag extra
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
  ls = list.mapi(fun (i, b) -> band(if i == 0 then 0. else list.nth(l,i-1).frequency end, b), l)
  c = add(normalize=false, ls)
  s =
    if not getter.is_constant(wet) or getter.get(wet) != 1. then
      add(normalize=false, [amplify({1.-getter.get(wet)}, s), amplify(wet, c)])
    else
      c
    end

  # Seal l element type
  if false then () else list.hd(l) end

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
# @flag extra
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
# @flag extra
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
# @flag extra
# @category Source / Audio processing
# @param ~power If `true` use constant power mixing.
# @param wetness Wetness coefficient, from 0 (fully dry) to  1 (fully wet).
# @param dry Dry source.
# @param wet Wet source.
def dry_wet(~power=false, wetness, dry, wet)
  add(power=power, weights=[getter.map(fun(x) -> 1.-x, wetness), wetness], [dry, wet])
end

# Generate DTMF tones.
# @flag extra
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
    l := blank(duration=delay) :: l()
    l := s :: l()
  end
  l = list.rev(l())
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
        volume = volume,
        selected = is_selected,
        single = is_single,
        source = s
      }
    end, sources)

  insert_metadata_fn = ref(fun (_) -> ())

  sources = list.map(fun (input) ->
    begin
      s = amplify(input.volume, input.source)
      s = source.on_track(s, fun (_) ->
         if input.single() then
           input.selected := false
         end
      )
      s = source.on_metadata(s, fun (m) ->
        begin
          fn = insert_metadata_fn()
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
                           input.volume := float_of_string(v)
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
                           input.selected := (b == "true")
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
                           input.single := (b == "true")
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

# Mixes two streams, with faded transitions between the state when only the
# normal stream is available and when the special stream gets added on top of
# it.
# @category Source / Fade
# @flag extra
# @param ~duration Duration of the fade in seconds.
# @param ~p       Portion of amplitude of the normal source in the mix.
# @param ~normal  The normal source, which could be called the carrier too.
# @param ~special The special source.
def smooth_add(~duration=1., ~p=getter(0.2), ~normal, ~special)
  p = getter.function(p)
  last_p = ref(p())

  def c(fn,s) =
    def v() =
      fn = fn()
      fn()
    end
    fade.scale(v,s)
  end

  special_volume = ref(fun () -> 0.)
  special = c(special_volume,special)

  normal_volume = ref(fun () -> 1.)
  normal = c(normal_volume,normal)

  def to_special(_,special) =
    last_p := p()
    q = 1. - last_p()
    normal_volume := mkfade(start=1.,stop=last_p(),duration=duration,normal)
    special_volume := mkfade(stop=q,duration=duration,special)
    special
  end

  def to_blank(special,b)
    normal_volume := mkfade(start=last_p(),duration=duration,normal)
    special_volume := mkfade(start=1.-last_p(),duration=duration,special)
    sequence([special,b])
  end

  special = fallback(track_sensitive=false,
               transitions=[to_special,to_blank],
               [special,blank()])

  add(normalize=false,[normal,special])
end

%ifencoder %ffmpeg
# Output an MPEG-DASH playlist.
# @category Source / Output
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~codec Codec to use for audio (following FFmpeg's conventions).
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~playlist Playlist name
# @param ~directory Directory to write to
def output.file.dash(~id=null(), ~fallible=false,
                     ~on_start={()}, ~on_stop={()}, ~codec="libmp3lame", ~bitrate=128,
                     ~start=true, ~playlist="stream.mpd", ~directory, s)
  enc = %ffmpeg(format="dash", %audio(codec=codec, b="#{bitrate}k"))
  output.file(id=id, fallible=fallible, on_start=on_start, on_stop=on_stop, start=start, enc, "#{(directory:string)}/#{playlist}", s)
end
%endif
