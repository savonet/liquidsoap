# Create an audio source from the given track, with
# metadata and track marks from that same track.
# @category Source / Track processing
def source.audio(~id=null("source.audio"), audio) =
  source(id=id,{
    audio=audio,
    metadata=track.metadata(audio),
    track_marks=track.track_marks(audio)
  })
end

# Create a video source from the given track, with
# metadata and track marks from that same track.
# @category Source / Track processing
def source.video(~id=null("source.video"), video) =
  source(id=id,{
    video=video,
    metadata=track.metadata(video),
    track_marks=track.track_marks(video)
  })
end

# Remove duplicate metadata in a track.
# @category Metadata
def track.metadata.deduplicate(~id=null("track.metadata.deduplicate"), t) =
  last_meta = ref([])
  def f(m) =
    if m == last_meta() then
      []
    else
      last_meta := m
      m
    end
  end
  track.metadata.map(id=id,insert_missing=false, update=false, strip=true, f, t)
end

# Remove duplicate metadata in a source.
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
  source(id=id, tracks.{metadata = track.metadata.map(%argsof(track.metadata.map), f, tracks.metadata)})
end

# Turn a source into an infaillible source by adding blank when the source is
# not available.
# @param s the source to turn infaillible
# @category Source / Track processing
def mksafe(~id="mksafe",s)
  fallback(id=id,track_sensitive=false,[(s:source),blank(id="safe_blank")])
end

# Creates a source that plays only one track of the input source.
# @category Source / Track processing
# @param s The input source.
def once(~id=null("once"), s)
  sequence(id=id, [(s:source),source.fail()])
end

# Skip track when detecting a blank.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param ~threshold Power in decibels under which the stream is considered silent.
# @param ~max_blank Maximum silence length allowed, in seconds.
# @param ~min_noise Minimum duration of noise required to end silence, in seconds.
# @param ~track_sensitive Reset blank counter at each track.
def blank.skip(~id=null("blank.skip"),~threshold=-40.,~max_blank=20.,~min_noise=0.,~track_sensitive=true,s)
  blank.detect(id=id,{s.skip()},threshold=threshold,max_blank=max_blank,min_noise=min_noise,track_sensitive=track_sensitive,s)
end

# Run a function regularly. This is similar to `thread.run` but based on a
# source internal time instead of the computer's time.
# @category Source / Track processing
# @param s Source whose time is taken as reference.
# @param ~delay Time to wait before the first run (in seconds).
# @param ~every How often to run the function (in seconds). The function is run once if `null`.
# @param f Function to run.
def source.run(s, ~delay=0., ~every=null(), f)
  next = ref(delay)
  def check()
    if source.time(s) >= next() then
      null.case(every, {next := infinity}, fun (every) -> next := next() + every)
      f()
    end
  end
  source.on_frame(s, check)
end
