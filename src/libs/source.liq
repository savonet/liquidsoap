# Mix sources, with optional normalization. Only relay metadata from the first available source. Track marks are dropped from all sources.
# @category Source / Audio processing
# @argsof track.audio.add
# @param ~weights Relative weight of the sources in the sum. The empty list stands for the homogeneous distribution. These are used as amplification coefficients if we are not normalizing.
def add(~id=null("add"), %argsof(track.audio.add[!id]), ~weights=[], sources) =
  if list.length(sources) == 0 then fallback(id=id, []) else
    tracks = list.map(source.tracks, sources)

    def mk_audio_track(pos, track) =
      weight =
        try
          list.nth(weights, pos)
        catch _ do
          getter(1.)
        end
      track.audio.{weight=weight}
    end

    audio = track.audio.add(%argsof(track.audio.add), list.mapi(mk_audio_track, tracks))

    new_tracks = {
      audio = audio,
      metadata = track.metadata(audio)
    }

    new_tracks =
      if null.defined(list.hd(tracks)?.video) then
         def mk_video_track(track) =
           null.get(track?.video)
         end
         new_tracks.{
           video = track.video.add(list.map(mk_video_track, tracks))
         }
      else
        new_tracks
      end

    source(id=id, new_tracks)
  end
end

# Create an audio source from the given track, with
# metadata and track marks from that same track.
# @category Source / Track processing
def source.audio(~id=null(), audio) =
  source(id=id,{
    audio=audio,
    metadata=track.metadata(audio),
    track_marks=track.track_marks(audio)
  })
end

# Create a video source from the given track, with
# metadata and track marks from that same track.
# @category Source / Track processing
def source.video(~id=null(), video) =
  source(id=id,{
    video=video,
    metadata=track.metadata(video),
    track_marks=track.track_marks(video)
  })
end

# Remove duplicate metadata in a track.
# @category Metadata
def track.metadata.deduplicate(t) =
  last_meta = ref([])
  def f(m) =
    if m == !last_meta then
      []
    else
      last_meta := m
      m
    end
  end
  track.metadata.map(insert_missing=false, update=false, strip=true, f, t)
end

# Remove duplicate metadata in a track.
# @category Metadata
# @param ~id Source id
# @param s source
def metadata.deduplicate(~id=null("metadata.deduplicate"), s) =
  tracks = source.tracks(s)
  source(id=id, tracks.{metadata = track.metadata.deduplicate(tracks.metadata)})
end

# Rewrite metadata on the fly using a function.
# @category Source / Track processing
# @argsof track.metadata.map
def metadata.map(~id=null("metadata.map"), %argsof(track.metadata.map[!id]), f, (s:source)) =
  tracks = source.tracks(s)
  source(tracks.{metadata = track.metadata.map(%argsof(track.metadata.map), f, tracks.metadata)})
end

# Turn a source into an infaillible source by adding blank when the source is
# not available.
# @param s the source to turn infaillible
# @category Source / Track processing
def mksafe(~id="mksafe",s)
  fallback(id=id,track_sensitive=false,[(s:source),blank(id="safe_blank")])
end

# Add a skip telnet command to a source when it does not have one by default.
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
  server.register(namespace="#{source.id(s)}",
                  usage="skip",
                  description="Skip the current song.",
                  "skip",skip)
end

# Creates a source that plays only one track of the input source.
# @category Source / Track processing
# @param s The input source.
def once(s)
  sequence([(s:source),source.fail()])
end

# Apply a function to the first track of a source
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param fn The applied function.
# @param s The input source.
def map_first_track(~id=null(),fn,s)
  fallback(id=id,track_sensitive=true,[fn((once(s):source)),s])
end

# Skip track when detecting a blank.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param ~threshold Power in decibels under which the stream is considered silent.
# @param ~max_blank Maximum silence length allowed, in seconds.
# @param ~min_noise Minimum duration of noise required to end silence, in seconds.
# @param ~track_sensitive Reset blank counter at each track.
def blank.skip(~id=null(),~threshold=-40.,~max_blank=20.,~min_noise=0.,~track_sensitive=true,s)
  blank.detect(id=id,{s.skip()},threshold=threshold,max_blank=max_blank,min_noise=min_noise,track_sensitive=track_sensitive,s)
end

# Same operator as rotate but merges tracks from each sources.
# For instance, `rotate.merge([intro,main,outro])` creates a source
# that plays a sequence `[intro,main,outro]` as single track and loops back.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
# @param ~weights Weights of the children (padded with 1), defining for each child how many tracks are played from it per round, if that many are actually available.
# @param sources Sequence of sources to be merged
def rotate.merge(~id=null(),~transitions=[],~weights=[],sources)
  ready = ref(true)
  duration = frame.duration()

  def to_first(_,new) =
    ready := (not !ready)
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

  switch(id=id,replay_metadata=false,track_sensitive=false,[({!ready}, s), ({(not !ready)}, s)])
end

# Rotate between overlapping sources. Next track starts according
# to 'liq_start_next' offset metadata.
# @category Source / Track processing
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
    fn = !send_to_main_source
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
# @category Metadata
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
# @category Source / Track processing
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
# @category Source / Track processing
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
# @category Source / Track processing
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
