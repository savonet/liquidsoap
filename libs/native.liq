# Native reimplementation of track functions.

let native = ()

# Create a source that plays only one track of the input source.
# @category Source / Track Processing
# @flag extra
def native.once(s)
  # available(track_sensitive=true, predicate.first({true}), (s:source))
  a = ref(true)
  source.on_track(s, fun(_) -> a := false)
  available({!a}, s)
end

# At the beginning of each track, select the first ready child.
# @category Source / Track Processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~track_sensitive Re-select only on end of tracks.
def native.fallback(~id="", ~track_sensitive=true, sources)
  fail = (fail():source)

  def s()
    list.find(default=fail, source.is_ready, sources)
  end

  infallible = not(list.for_all(source.fallible, sources))
  source.dynamic(id=id, infallible=infallible, track_sensitive=track_sensitive, s)
end

# Play only one track of every successive source, except for the last one which
# is played as much as available.
# @category Source / Track Processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param sources List of sources to play tracks from.
def native.sequence(~id="", sources)
  len  = list.length(sources)
  n    = ref(0)
  fail = fail()

  def rec s()
    sn = list.nth(default=list.last(default=fail, sources), sources, !n)
    if sn.is_ready() or !n >= len - 1 then
      sn
    else
      ref.incr(n)
      s()
    end
  end
  infallible = not list.last(default=fail, sources).fallible()
  s = source.dynamic(infallible=infallible, track_sensitive=true, s)

  first = ref(true)
  def ot(_)
    # Drop the first track
    if !first then first := false else
      ref.incr(n)
    end
  end

  source.on_track(s, ot)
  s
end

# Select the first source whose predicate is true in a list. If the second
# argument is a getter, the source will be dynamically created.
# @category Source / Track Processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~track_sensitive Re-select only on end of tracks.
# @param sources Sources with the predicate telling when they can be played.
def native.switch(~id="", ~track_sensitive=true, sources)
  fail = (fail():source)
  def s()
    def activated(ps)
      let (p, s) = ps
      if p () then
        s = getter.get(s)
        if source.is_ready(s) then s else null () end
      else
        null()
      end
    end
    null.find(default=fail, activated, sources)
  end
  source.dynamic(track_sensitive=track_sensitive, s)
end
