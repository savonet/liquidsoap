# Turn a source into an infaillible source by adding blank when the source is
# not available.
# @param s the source to turn infaillible
# @category Source / Track Processing
def mksafe(~id="mksafe",s)
  fallback(id=id,track_sensitive=false,[s,(blank(id="safe_blank"):source)])
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

# Creates a source that plays only one track of the input source.
# @category Source / Track Processing
# @param s The input source.
def once(s)
  sequence([s,(source.fail():source)])
end

# Apply a function to the first track of a source
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param fn The applied function.
# @param s The input source.
def map_first_track(~id=null(),fn,s)
  fallback(id=id,track_sensitive=true,[fn((once(s):source)),s])
end

# Skip track when detecting a blank.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~threshold Power in decibels under which the stream is considered silent.
# @param ~max_blank Maximum silence length allowed, in seconds.
# @param ~min_noise Minimum duration of noise required to end silence, in seconds.
# @param ~track_sensitive Reset blank counter at each track.
def blank.skip(~id=null(),~threshold=-40.,~max_blank=20.,~min_noise=0.,~track_sensitive=true,s)
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
def rotate.merge(~id=null(),~transitions=[],~weights=[],sources)
  ready = ref(true)
  duration = frame.duration()

  def to_first(old,new) =
    ignore((old:source))
    ignore((new:source))
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

# Rotate between overlapping sources. Next track starts according
# to 'liq_start_next' offset metadata.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~start_next Metadata field indicating when the next track should start, relative to current track's time.
# @param ~weights Relative weight of the sources in the sum. The empty list stands for the homogeneous distribution.
# @param sources Sources to toggle from
def overlap_sources(~id=null(),~normalize=false,
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

# Append speech-synthesized tracks reading the metadata.
# @category Source / Track Processing
# @param ~pattern Pattern to use
# @param s The source to use
def source.say_metadata =
  def pattern(m) =
    artist = m["artist"]
    title = m["title"]
    artist_predicate =
      if artist != "" then
        "It was #{artist} playing "
      else
        ""
      end
    "say:#{artist_predicate}#{title}"
  end
  fun (~id=null(), ~pattern=pattern, s) ->
    append(id=id, s, fun (m) -> once(single(pattern(m))))
end

# Run a function regularly. This is similar to `thread.run` but based on a
# source internal time instead of the world's time.
# @category Source / Track Processing
# @param s Source whose time is taken as reference.
# @param ~delay Time to wait before the first run (in seconds).
# @param ~every How often to run the function (in seconds). The function is run once if `null`.
# @param f Function to run.
def source.run(s, ~delay=0., ~every=null(), f)
  next = ref(delay)
  def check()
    if source.time(s) >= !next then
      null.case(every, {next := infinity}, fun (every) -> next := !next + every)
      f()
    end
  end
  source.on_frame(s, check)
end

# Regularly insert track boundaries in a stream (useful for testing tracks).
# @category Source / Track Processing
# @param ~every Duration of a track (in seconds).
# @param ~metadata Metadata for tracks.
# @param s The stream.
def chop(~every=getter(3.), ~metadata=getter([]), s) =
  s = insert_metadata(s)

  # Track time in the source's context:
  start_time = ref(0.)
  def f() =
    if getter.get(every) <= s.time() - !start_time then
      start_time := s.time()
      s.insert_metadata(new_track=true, getter.get(metadata))
    end
  end

  source.on_frame(s, f)
end

# Regularly skip tracks from a source (useful for testing skipping).
# @category Source / Track Processing
# @param ~every How often to skip tracks.
# @param s The stream.
# @flag extra
def skipper(~every=getter(5.), s) =
  start_time = ref(0.)
  def f() =
    if getter.get(every) <= s.time() - !start_time then
      start_time := s.time()
      s.skip()
    end
  end

  source.on_frame(s, f)
end

# Indicate beats.
# @param ~frequency Frequency of the sound.
# @param bpm Number of beats per minute.
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
