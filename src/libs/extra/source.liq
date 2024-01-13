# Apply a function to the first track of a source
# @category Source / Track processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param fn The applied function.
# @param s The input source.
def map_first_track(~id=null("map_first_track"), fn, s) =
  fallback(id=id, track_sensitive=true, [fn((once(s) : source)), s])
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
def rotate.merge(
  ~id=null("rotate.merge"),
  ~transitions=[],
  ~weights=[],
  sources
) =
  ready = ref(true)
  duration = frame.duration()

  def to_first(_, new) =
    ready := (not ready())
    sequence(merge=true, [blank(duration=duration), (new : source)])
  end

  transitions =
    if
      list.length(transitions) == 0
    then
      [to_first]
    else
      list.mapi(
        (
          fun (i, t) ->
            if
              i == 0
            then
              (fun (old, new) -> to_first(old, t(old, new)))
            else
              t
            end
        ),
        transitions
      )
    end

  s = rotate(transitions=transitions, weights=weights, sources)
  let {track_marks = _, ...tracks} = source.tracks(s)
  s = source(tracks)
  switch(
    id=id,
    replay_metadata=false,
    track_sensitive=false,
    [(ready, s), ({not ready()}, s)]
  )
end

# Rotate between overlapping sources. Next track starts according
# to 'liq_start_next' offset metadata.
# @category Source / Track processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~start_next Metadata field indicating when the next track should start, relative to current track's time.
# @param ~weights Relative weight of the sources in the sum. The empty list stands for the homogeneous distribution.
# @param sources Sources to toggle from
def overlap_sources(
  ~id=null("overlap_sources"),
  ~normalize=false,
  ~start_next="liq_start_next",
  ~weights=[],
  sources
) =
  position = ref(0)
  length = list.length(sources)

  def current_position() =
    pos = position()
    position := (pos + 1) mod length
    pos
  end

  ready_list = list.map(fun (_) -> ref(false), sources)
  grab_ready = fun (n) -> list.nth(default=ref(false), ready_list, n)

  def set_ready(pos, b) =
    is_ready = grab_ready(pos)
    is_ready := b
  end

  # Start next track on_offset
  def on_start_next(_, _) =
    set_ready(current_position(), true)
  end

  def on_offset(s) =
    let (s, offset) = metadata.getter.source.float(-1., start_next, s)
    on_offset(force=true, offset=offset, on_start_next, s)
  end

  sources = list.map(on_offset, sources)

  # Disable after each track
  def disable(pos, source) =
    def disable(_) =
      set_ready(pos, false)
    end

    source.on_track(disable)
  end

  list.iteri(disable, sources)

  # Relay metadata from all sources
  send_to_main_source = ref(fun (_) -> ())

  def relay_metadata(m) =
    fn = send_to_main_source()
    fn(m)
  end

  list.iter(fun (s) -> s.on_metadata(relay_metadata), sources)

  def drop_metadata(s) =
    let {metadata = _, ...tracks} = source.tracks(s)
    source(tracks)
  end

  # Now drop all metadata
  sources = list.map(drop_metadata, sources)

  # Wrap sources into switches.
  def make_switch(pos, source) =
    is_ready = grab_ready(pos)
    switch(track_sensitive=true, [(is_ready, source)])
  end

  sources = list.mapi(make_switch, sources)

  # Initiate the whole thing.
  set_ready(current_position(), true)

  # Create main source
  source = add(id=id, normalize=normalize, weights=weights, sources)

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
      if
        artist != ""
      then
        "It was #{artist} playing "
      else
        ""
      end

    say_metadata = "#{artist_predicate}#{title}"
    say_metadata = r/:/g.replace(fun (_) -> '$(colon)', say_metadata)
    say_metadata =
      say_metadata == ""
      ?
        "Sorry, I do not know what this song title was"
      : say_metadata

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
  time = ref(0.)

  def f() =
    time := time() + settings.frame.duration()
    if
      getter.get(every) <= time()
    then
      time := 0.
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
    if
      getter.get(every) <= s.time() - start_time()
    then
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
def fallback.skip(s, ~fallback=null()) =
  fallback = fallback ?? (blank() : source)
  avail = ref(true)

  def check() =
    old = avail()
    avail := source.is_ready(s)
    if not old and avail() then source.skip(fallback) end
  end

  s = stdlib_fallback(track_sensitive=false, [s, fallback])

  # TODO: could we have something more efficient that checking on every frame
  source.on_frame(s, check)
end

stdlib_file = file

# Generate a CUE file for the source. This function will generate a new track in
# the file for each metadata of the source.
# @category Source / Track processing
# @flag extra
# @param filename Path where the CUE file should be written.
# @param max_length Set the maximum of track info retained by the function. \
#                   Infinite when `null`.
# @param ~title Title of the stream.
# @param ~file File where the stream is stored.
# @param ~file_type Format in which the stream is stored.
# @param ~comment Comment about the stream.
# @param ~date Year for the stream.
# @param ~map_metadata Function to apply to metadata before writing the CUE file (useful for pre-processing metadata).
# @param ~temp_dir Temporary directory for atomic write.
# @param ~delete Delete the CUE files when starting if it exists.
def source.cue(
  ~title=null(),
  ~performer=null(),
  ~file=null(),
  ~file_type="mp3",
  ~comment=null(),
  ~date=null(),
  ~map_metadata=fun (m) -> m,
  ~max_length=null(),
  ~temp_dir=null(),
  ~delete=true,
  filename,
  s
) =
  if
    delete and stdlib_file.exists(filename)
  then
    stdlib_file.remove(filename)
  end

  is_first = ref(true)

  def write(~append, entries) =
    if
      append
    then
      log(
        label="source.cue",
        level=4,
        "Writing new entry to #{filename}"
      )
    else
      log(
        label="source.cue",
        level=4,
        "Writing full CUE file at #{filename}"
      )
    end

    write =
      stdlib_file.write.stream(
        append=append, atomic=true, temp_dir=temp_dir, filename
      )

    # Append to the file.
    def w(data) =
      write(data ^ "\n")
    end

    # Write a tag.
    def tag(~indent=0, name, (value:string?)) =
      if
        null.defined(value)
      then
        s =
          "#{string.spaces(indent)}#{name} #{string.quote(null.get(value))}"
        w(s)
      end
    end

    if
      is_first() or not append
    then
      is_first := false

      tag("TITLE", title)
      tag("PERFORMER", performer)
      tag(
        "REM COMMENT",
        comment
      )
      if
        null.defined(date)
      then
        w(
          "DATE #{(null.get(date) : int)}"
        )
      end
      if
        null.defined(file)
      then
        w(
          "FILE \"#{null.get(file)}\" #{string.uppercase(file_type)}"
        )
      end
    end

    list.iter(
      fun (entry) ->
        begin
          let {position = p, time = t, metadata = m} = entry

          w(
            "  TRACK #{string.of_int(digits=2, p)} AUDIO"
          )
          tag(indent=4, "TITLE", list.assoc.nullable("title", m))
          tag(indent=4, "PERFORMER", list.assoc.nullable("artist", m))
          tag(
            indent=4,
            "REM ALBUM",
            list.assoc.nullable("album", m)
          )

          frames = int_of_float((t - floor(t)) * 75.)
          t = int_of_float(t)
          minutes = t / 60
          seconds = t mod 60
          m = string.of_int(digits=2, minutes)
          s = string.of_int(digits=2, seconds)
          f = string.of_int(digits=2, frames)
          w(
            "    INDEX 01 #{m}:#{s}:#{f}"
          )
        end,
      entries
    )

    write("")
  end

  entries = ref([])
  current_position = ref(1)
  t0 = source.time(s)

  def handle_metadata(m) =
    m = map_metadata(m)
    entry = {position=current_position(), time=source.time(s) - t0, metadata=m}
    ref.incr(current_position)

    if
      null.defined(max_length)
    then
      current_entries =
        null.case(
          max_length,
          entries,
          fun (max_length) ->
            list.rev(list.prefix(max_length - 1, list.rev(entries())))
        )
      entries :=
        list.mapi(
          fun (pos, entry) -> entry.{position=pos + 1},
          [...current_entries, entry]
        )
      write(append=false, entries())
    else
      write(append=true, [entry])
    end
  end

  source.on_track(s, handle_metadata)
end
