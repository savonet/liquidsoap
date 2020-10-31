# Turn a source into an infaillible source by adding blank when the source is
# not available.
# @param s the source to turn infaillible
# @category Source / Track Processing
def mksafe(~id="mksafe",s)
  fallback(id=id,track_sensitive=false,[s,blank(id="safe_blank")])
end

# Add a skip function to a source when it does not have one by default.
# @category Interaction
# @flag extra
# @param s The source to attach the command to.
def add_skip_command(s) =
  # A command to skip
  def skip(_) =
    s.skip()
    "Done!"
  end
  # Register the command:
  server.register(namespace="#{s.id()}",
                  usage="skip",
                  description="Skip the current song.",
                  "skip",skip)
end

# Removes all metadata coming from a source.
# @category Source / Track Processing
def drop_metadata(s)
  map_metadata(fun(_)->[],update=false,strip=true,insert_missing=false,s)
end

# Default inputs and outpus.
#
# They are called "prefered" but it's not a user preference, just a view of
# what's generally preferable among the available modules.
#
# It is important that input and output preferences are in the same order: the
# chosen I/O should work in the same clock, we don't want an ALSA input and OSS
# output. The only exception is AO: it is the default output after dummy, so the
# input will be a dummy when AO is used for output.

let replaces output=output.dummy
%ifdef output.ao
  let replaces output=output.ao
%endif
%ifdef output.alsa
  let replaces output=output.alsa
%endif
%ifdef output.oss
  let replaces output=output.oss
%endif
%ifdef output.portaudio
  let replaces output=output.portaudio
%endif
%ifdef output.pulseaudio
  let replaces output=output.pulseaudio
%endif
# Output to local audio card using the first available driver in pulseaudio,
# portaudio, oss, alsa, ao, dummy.
# @category Source / Output
def replaces output(~id="",~fallible=false,
           ~on_start={()},~on_stop={()},~start=true,s)
  output(id=id,fallible=fallible,
         start=start,on_start=on_start,on_stop=on_stop,
         s)
end

def replaces input(~id="",~start=true,~on_start={()},~on_stop={()},~fallible=false)
  blank(id=id)
end
%ifdef input.alsa
  let replaces input = input.alsa
%endif
%ifdef input.oss
  let replaces input = input.oss
%endif
%ifdef input.portaudio
  let replaces input = input.portaudio
%endif
%ifdef input.pulseaudio
  let replaces input = input.pulseaudio
%endif

# Create a source from the first available input driver in pulseaudio,
# portaudio, oss, alsa, blank.
# @category Source / Input
def replaces input(~id="",~start=true,~on_start={()},~on_stop={()},~fallible=false)
  input(id=id,start=start,on_start=on_start,on_stop=on_stop,fallible=fallible)
end

# Special track insensitive fallback that always skips current song before
# switching.
# @category Source / Track Processing
# @param ~input The input source
# @flag extra
# @param f The fallback source
def fallback.skip(~input,f)
  def transition(a,b) =
    a.skip()
    # This eats the last remaining frame from a
    sequence([a,b])
  end
  fallback(track_sensitive=false,transitions=[transition,transition],[input,f])
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

%ifdef soundtouch
# Increases the pitch, making voices sound like on helium.
# @category Source / Sound Processing
# @flag extra
# @param s The input source.
def helium(s)
  soundtouch(pitch=1.5,s)
end
%endif

# Get the value associated to a variable in the process environment. Return ""
# if variable is not set.
# @category System
def getenv(s) =
  list.assoc(default="",s,environment())
end

# Split the arguments of an url of the form arg=bar&arg2=bar2 into
# [("arg","bar"),("arg2","bar2")].
# @category String
# @param args Agument string to split
def url.split_args(args) =
  def f(x) =
    ret = string.split(separator="=",x)
    arg = url.decode(list.nth(default="",ret,0))
    val = if list.length(ret) == 1 then "" else url.decode(list.nth(default="",ret,1)) end
    (arg,val)
  end
  l = string.split(separator="&",args)
  list.map(f,l)
end

# Split an url of the form foo?arg=bar&arg2=bar2 into
# ("foo",[("arg","bar"),("arg2","bar2")]).
# @category String
# @param uri Url to split
def url.split(uri) =
  ret = string.extract(pattern="([^\\?]*)\\?(.*)",uri)
  args = ret[2]
  if args != "" then
    (ret[1],url.split_args(ret[2]))
  else
    (uri,[])
  end
end

# Register a server/telnet command to update a source's metadata. Returns a new
# source, which will receive the updated metadata. The command has the following
# format: insert key1="val1",key2="val2",...
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
def server.insert_metadata(~id="",s) =
  s = insert_metadata(id=id,s)
  insert = s.insert_metadata
  def insert(s) =
    #TODO: we don't split as expected if one string contains a comma, but this
    #is difficult to achieve with regexps
    l = string.split(separator=',',s)
    def f(l,x) =
      def sub(s) = string.replace(pattern='\\"',fun (_) -> '"',s) end
      if x != "" then
        ret = string.extract(pattern='([^=]+)\\s*=\\s*"((?:\\"|[^"])*)"',x)
        if ret[1] != "" then
          list.append(l, [(ret[1], sub(ret[2]))])
        else
          l
        end
      else
        l
      end
    end
    meta = list.fold(f,[],l)
    if meta != [] then
      insert(meta)
      "Done"
    else
      "Syntax error or no metadata given. Use key1=\"val1\",key2=\"val2\",.."
    end
  end
  id = s.id()
  server.register(namespace="#{id}",
                  description="Insert a metadata chunk.",
                  usage="insert key1=\"val1\",key2=\"val2\",..",
                  "insert",insert)
  s
end

# Register a command that outputs the RMS of the returned source.
# @category Source / Visualization
# @param ~id Force the value of the source ID.
def server.rms(~id="",s) =
  let s = rms(id=id,s)
  id = s.id()
  def rms(_) =
    rms = s.rms()
    "#{rms}"
  end
  server.register(namespace="#{id}",
                  description="Return the current RMS of the source.",
                  usage="rms",
                  "rms",rms)
  s
end

# Remove low frequencies often produced by microphones.
# @category Source / Sound Processing
# @param s The input source.
def mic_filter(s)
  filter(freq=200.,q=1.,mode="high",s)
end

# Creates a source that plays only one track of the input source.
# @category Source / Track Processing
# @param s The input source.
def once(s)
  sequence([s,fail()])
end

# Apply a function to the first track of a source
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param fn The applied function.
# @param s The input source.
def map_first_track(~id="",fn,s)
  fallback(id=id,track_sensitive=true,[fn(once(s)),s])
end

# Restrict a source to play only when a predicate is true.
# @category Source / Track Processing
# @param pred The predicate, typically a time interval such as \
#             `{10h-10h30}`.
def at(pred,s)
  switch([(pred,s)])
end

# Assign a new clock to the given source (and to other time-dependent sources)
# and return the source. It is a conveniency wrapper around clock.assign_new(),
# allowing more concise scripts in some cases.
# @category Liquidsoap
# @param ~sync Synchronization mode. One of: `"auto"`, `"cpu"`, or `"none"`. Defaults to `"auto"`, which synchronizes with the CPU clock if none of the active sources are attached to their own clock (e.g. ALSA input, etc). `"cpu"` always synchronizes with the CPU clock. `"none"` removes all synchronization control.
def replaces clock(~sync="auto",~id="",s)
  clock.assign_new(sync=sync,id=id,[s])
  s
end

# Create a log of clock times for all the clocks initially present. The log is
# in a simple format which you can directly use with gnuplot.
# @category Liquidsoap
# @param ~delay    Delay before setting up the clock logger. This should \
#                  be used to ensure that the logger starts only after \
#                  the clocks are created.
# @param ~every    Polling interval.
# @param logfile   Path of the log file.
def clock.log(~delay=0., ~every=1., logfile)
  # Get the current clocks
  clocks = list.map(fst,clock.status())
  # Column headers
  ignore(file.write(data="# #{string.concat(separator=' ', clocks)}", logfile))
  def report()
    status = clock.status()
    status = list.map(fun (x) -> (fst(x),string_of(snd(x))), status)
    status = list.map(fun (c) -> status[c], clocks)
    ignore(file.write(append=true, data="#{string.concat(separator=' ', status)}", logfile))
  end
  delay = if delay <= 0. then 0. else delay end
  thread.run(delay=delay, every=every, report)
end

# Skip track when detecting a blank.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~threshold Power in decibels under which the stream is considered silent.
# @param ~max_blank Maximum silence length allowed, in seconds.
# @param ~min_noise Minimum duration of noise required to end silence, in seconds.
# @param ~track_sensitive Reset blank counter at each track.
def blank.skip(~id="",~threshold=-40.,~max_blank=20.,~min_noise=0.,~track_sensitive=true,s)
  blank.detect({s.skip()},threshold=threshold,max_blank=max_blank,min_noise=min_noise,track_sensitive=track_sensitive,s)
end

# Same operator as rotate but merges tracks from each sources.
# For instance, `rotate.merge([intro,main,outro])` creates a source
# that plays a sequence `[intro,main,outro]` as single track and loops back.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
# @param ~weights Weights of the children (padded with 1), defining for each child how many tracks are played from it per round, if that many are actually available.
# @param sources Sequence of sources to be merged
def rotate.merge(~id="",~transitions=[],~weights=[],sources)
  ready = ref(true)
  duration = get(default=0.04,"frame.duration")

  def to_first(old,new) =
    ready := (not !ready)
    sequence(merge=true,[blank(duration=duration),new])
  end

  transitions = if list.length(transitions) == 0 then
    [to_first]
  else
    list.mapi((fun (i,t) ->
      if i == 0 then
        (fun (old,new) ->
          to_first(old,t(old,new)))
      else t end), transitions)
  end

  source = rotate(transitions=transitions,weights=weights,sources)
  source = merge_tracks(source)

  switch(id=id,replay_metadata=false,track_sensitive=false,[({!ready}, source), ({(not !ready)}, source)])
end

# Extract the left channel of a stereo source
# @category Source / Conversions
# @param s Source to extract from
def stereo.left(s)
  mean(stereo.pan(pan=-1., s))
end

# Extract the right channel of a stereo source
# @category Source / Conversions
# @param s Source to extract from
def stereo.right(s)
  mean(stereo.pan(pan=1., s))
end

# Append speech-synthesized tracks reading the metadata.
# @category Source / Track Processing
# @param ~pattern Pattern to use
# @param s The source to use
def say_metadata
  p = 'say:$(if $(artist),"It was $(artist)$(if $(title),\\", $(title)\\").")'
  fun (s,~pattern=p) ->
    append(s, fun (m) -> once(single(pattern % m)))
end

# Rotate between overlapping sources. Next track starts according
# to 'liq_start_next' offset metadata.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~start_next Metadata field indicating when the next track should start, relative to current track's time.
# @param ~weights Relative weight of the sources in the sum. The empty list stands for the homogeneous distribution.
# @param sources Sources to toggle from
def overlap_sources(~id="",~normalize=false,
                    ~start_next="liq_start_next",
                    ~weights=[],sources) =
  position = ref(0)
  length   = list.length(sources)

  def current_position() =
    pos = !position
    position := (pos + 1) mod length
    pos
  end

  ready_list = list.map(fun (_) -> ref(false),sources)
  grab_ready = list.nth(default=ref(false),ready_list)

  def set_ready(pos,b) =
    is_ready = grab_ready(pos)
    is_ready := b
  end

  # Start next track on_offset
  def on_start_next(_,_) =
    set_ready(current_position(),true)
  end
  on_offset = on_offset(force=true,override=start_next,on_start_next)
  sources = list.map(on_offset,sources)

  # Disable after each track
  def disable(pos,source) =
    def disable(_) =
      set_ready(pos,false)
    end
    source.on_track(disable)
  end

  list.iteri(disable,sources)

  # Relay metadata from all sources
  send_to_main_source = ref(fun (_) -> ())

  def relay_metadata(m) =
    fn = !send_to_main_source
    fn(m)
  end
  list.iter(fun (s) -> s.on_metadata(relay_metadata),sources)

  # Now drop all metadata
  sources = list.map(drop_metadata,sources)

  # Wrap sources into switches.
  def make_switch(pos,source) =
    is_ready = grab_ready(pos)
    switch(track_sensitive=true,[({!is_ready},source)])
  end
  sources = list.mapi(make_switch,sources)

  # Initiate the whole thing.
  set_ready(current_position(),true)

  # Create main source
  source = add(id=id,normalize=normalize,weights=weights,sources)

  # Set send_to_main_source
  s = insert_metadata(source)
  send_to_main_source := fun (m) -> s.insert_metadata(m)
  s
end

# Restart one server client waiting on the given condition
# @param c condition
# @category Interaction
def server.signal(c) =
  signal = fst(snd(c))
  signal()
end

# Restart all server clients waiting on the given condition
# @param c condition
# @category Interaction
def server.broadcast(x) =
  broadcast = snd(snd(x))
  broadcast()
end

# Regularly insert track boundaries in a stream (useful for testing tracks).
# @param ~duration Duration of a track (in seconds).
# @param ~metadata Metadata for tracks.
# @param s The stream.
# @category Source / Track Processing
def chop(~duration=float_getter(3.),~metadata=[],s) =
  duration=to_float_getter(duration)
  s = insert_metadata(s)

  # Track time in the source's context:
  start_time = ref(0.)
  def f() =
    if duration () <= s.time() - !start_time then
      start_time := s.time()
      s.insert_metadata(new_track=true, metadata)
    end
    1.
  end

  amplify(f,s)
end

# Make a source available or not depending on a boolean.
# @param ~id The source ID
# @param ~active Whether the source should be pulled when not available
# @param ~track_sensitive Re-select only on end of tracks.
# @param ~available Whether the source is available (`true`) or not (`false`).
# @param s The stream.
# @category Source / Track Processing
def mkavailable(~id="mkavailable",~track_sensitive=bool_getter(true),~active=bool_getter(false),~available=bool_getter(true),s) =
  output.dummy(switch([(to_bool_getter(active),s)]),fallible=true)

  switch(id=id,track_sensitive=to_bool_getter(track_sensitive),[(to_bool_getter(available),s)])
end
