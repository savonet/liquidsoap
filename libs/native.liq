# Native reimplementation of track functions.

let native = ()

# Create a source that plays only one track of the input source.
# @category Source / Track Processing
# @flag extra
def native.once(s)
  # source.available(track_sensitive=true, (s:source), predicate.first({true}))
  a = ref(true)
  source.on_track(s, fun(_) -> a := false)
  source.available(s, {!a})
end

# At the beginning of each track, select the first ready child.
# @category Source / Track Processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~track_sensitive Re-select only on end of tracks.
def native.fallback(~id=null(), ~track_sensitive=true, sources)
  fail = (source.fail():source)

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
def native.sequence(~id=null(), sources)
  len  = list.length(sources)
  n    = ref(0)
  fail = (source.fail():source)

  def rec s()
    sn = list.nth(default=list.last(default=fail, sources), sources, !n)
    if source.is_ready(sn) or !n >= len - 1 then
      sn
    else
      ref.incr(n)
      s()
    end
  end
  infallible = not source.fallible(list.last(default=fail, sources))
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
def native.switch(~id=null(), ~track_sensitive=true, sources)
  sources = list.map(fun(ps) -> source.available(snd(ps), fst(ps)), sources)
  native.fallback(id=id, track_sensitive=track_sensitive, sources)
end

# This allows doing `open native`
let native.request = request

# Play requests dynamically created by a given function.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~polling_delay How often the queue should be checkelid and filled in seconds.
# @param ~prefetch Number of tracks to prepare in advance.
# @param ~timeout Timeout for resolving requests in seconds.
# @param f Function returning the next request.
def native.request.dynamic(~id=null(), ~prefetch=1, ~polling_delay=1., ~timeout=20., f)
  # Prepared requests
  queue = ref([])

  def get_queue() =
    def get_request(s) =
      s.request
    end
    list.map(get_request, !queue)
  end

  def set_queue(q) =
    def mk_source(result, r) =
      s = request.once(r)
      if s.resolve() then
        [...result, s]
      else
        result
      end
    end
    queue := list.fold(mk_source, [], q)
  end

  current = ref(null())

  def get_current() =
    if null.defined(!current) then
      s = null.get(!current)
      s.request
    else
      null()
    end
  end

  # Prefetch thread
  def prefetch()
    if list.length(!queue) < prefetch then
      r = f()
      if null.defined(r) then
        r = null.get(r)
        s = request.once(r)
        if s.resolve() then
          log.info("Added request on queue: #{request.uri(r)}.")
          queue := [...!queue, s]
        else
          log.important("Failed to resolve request #{request.uri(r)}.")
        end
      end
    end
  end
  thread.run(every=polling_delay, prefetch)

  # Source
  def s()
    if not list.is_empty(!queue) then
      let [s, ...rest] = !queue
      queue := rest
      current := s
      s
    else
      source.fail()
    end
  end
  source.dynamic(id=id, track_sensitive=true, s).{
    prefetch=prefetch,
    queue=get_queue,
    set_queue=set_queue,
    current=get_current
  }
end

# Play requests dynamically created by a given function.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~polling_delay How often the queue should be checkelid and filled in seconds.
# @param ~prefetch Number of tracks to prepare in advance.
# @param ~timeout Timeout for resolving requests in seconds.
# @param f Function returning the list of next requests.
def native.request.dynamic.list(~id=null(), ~prefetch=1, ~polling_delay=1., ~timeout=20., f)
  l = ref([])
  def f()
    if !l == [] then l := f() end
    if !l != [] then
      r = list.hd(!l)
      l := list.tl(!l)
      r
    else
      null()
    end
  end
  native.request.dynamic(id=id, f)
end
