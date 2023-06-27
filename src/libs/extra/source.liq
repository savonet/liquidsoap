# Apply a function to the first track of a source
# @category Source / Track processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param fn The applied function.
# @param s The input source.
def map_first_track(~id=null("map_first_track"),fn,s)
  fallback(id=id,track_sensitive=true,[fn((once(s):source)),s])
end

# Same operator as rotate but merges tracks from each sources.
# For instance, `rotate.merge([intro,main,outro])` creates a source
# that plays a sequence `[intro,main,outro]` as single track and loops back.
# @category Source / Track processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
# @param ~weights Weights of the children (padded with 1), defining for each child how many tracks are played from it per round, if that many are actually available.
# @param sources Sequence of sources to be merged
def rotate.merge(~id=null("rotate.merge"),~transitions=[],~weights=[],sources)
  ready = ref(true)
  duration = frame.duration()

  def to_first(_,new) =
    ready := (not ready())
    sequence(merge=true,[blank(duration=duration),(new:source)])
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

  s = rotate(transitions=transitions,weights=weights,sources)
  let {track_marks=_, ...tracks} = source.tracks(s)
  s = source(tracks)

  switch(id=id,replay_metadata=false,track_sensitive=false,[(ready, s), ({not ready()}, s)])
end


# Rotate between overlapping sources. Next track starts according
# to 'liq_start_next' offset metadata.
# @category Source / Track processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~start_next Metadata field indicating when the next track should start, relative to current track's time.
# @param ~weights Relative weight of the sources in the sum. The empty list stands for the homogeneous distribution.
# @param sources Sources to toggle from
def overlap_sources(~id=null("overlap_sources"),~normalize=false,
                    ~start_next="liq_start_next",
                    ~weights=[],sources) =
  position = ref(0)
  length   = list.length(sources)

  def current_position() =
    pos = position()
    position := (pos + 1) mod length
    pos
  end

  ready_list = list.map(fun (_) -> ref(false),sources)
  grab_ready = fun(n) -> list.nth(default=ref(false),ready_list,n)

  def set_ready(pos,b) =
    is_ready = grab_ready(pos)
    is_ready := b
  end

  # Start next track on_offset
  def on_start_next(_,_) =
    set_ready(current_position(),true)
  end
  on_offset = fun(s) -> on_offset(force=true,override=start_next,on_start_next,s)
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
    fn = send_to_main_source()
    fn(m)
  end
  list.iter(fun (s) -> s.on_metadata(relay_metadata),sources)

  def drop_metadata(s) =
    let {metadata=_, ...tracks} = source.tracks(s)
    source(tracks)
  end

  # Now drop all metadata
  sources = list.map(drop_metadata,sources)

  # Wrap sources into switches.
  def make_switch(pos,source) =
    is_ready = grab_ready(pos)
    switch(track_sensitive=true,[(is_ready,source)])
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
# @category Metadata
# @flag extra
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
    say_metadata = "#{artist_predicate}#{title}"
    say_metadata = r/:/g.replace(fun (_) -> '$(colon)', say_metadata)
    "say:#{say_metadata}"
  end
  fun (~id=null("source.say_metadata"), ~pattern=pattern, s) ->
    append(id=id, s, fun (m) -> once(single(pattern(m))))
end

# Regularly insert track boundaries in a stream (useful for testing tracks).
# @category Source / Track processing
# @flag extra
# @param ~every Duration of a track (in seconds).
# @param ~metadata Metadata for tracks.
# @param s The stream.
def chop(~every=getter(3.), ~metadata=getter([]), s) =
  s = insert_metadata(s)

  # Track time in the source's context:
  start_time = ref(0.)
  def f() =
    if getter.get(every) <= s.time() - start_time() then
      start_time := s.time()
      s.insert_metadata(new_track=true, getter.get(metadata))
    end
  end

  source.on_frame(s, f)
end

# Regularly skip tracks from a source (useful for testing skipping).
# @category Source / Track processing
# @flag extra
# @param ~every How often to skip tracks.
# @param s The stream.
# @flag extra
def skipper(~every=getter(5.), s) =
  start_time = ref(0.)
  def f() =
    if getter.get(every) <= s.time() - start_time() then
      start_time := s.time()
      s.skip()
    end
  end

  source.on_frame(s, f)
end

let stdlib_fallback = fallback

# Special track insensitive fallback that always skips current song before
# switching.
# @category Source / Track processing
# @flag extra
# @param s The main source.
# @param ~fallback The fallback source. Defaults to `blank` if `null`.
def fallback.skip(s,~fallback=null())
  fallback = fallback ?? (blank():source)
  avail = ref(true)
  def check()
    old = avail()
    avail := source.is_ready(s)
    if not old and avail() then
      source.skip(fallback)
    end
  end
  s = stdlib_fallback(track_sensitive=false, [s, fallback])
  # TODO: could we have something more efficient that checking on every frame
  source.on_frame(s, check)
end
