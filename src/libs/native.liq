# Native reimplementation of track functions.

let native = ()

# Create a source that plays only one track of the input source.
# @category Source / Track processing
# @flag extra
def native.once(s)
  # source.available(track_sensitive=true, s, predicate.first({true}))
  a = ref(true)
  s = source.on_track(s, fun(_) -> a := false)
  source.available(s, a)
end

# At the beginning of each track, select the first ready child.
# @category Source / Track processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~track_sensitive Re-select only on end of tracks.
def native.fallback(~id=null(), ~track_sensitive=true, sources)
  fail = (source.fail() : source)

  def s()
    list.find(default=fail, source.is_ready, sources)
  end

  infallible = not(list.for_all(source.fallible, sources))
  source.dynamic(id=id, infallible=infallible, track_sensitive=track_sensitive, s)
end

# Play only one track of every successive source, except for the last one which
# is played as much as available.
# @category Source / Track processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param sources List of sources to play tracks from.
def native.sequence(~id=null(), sources)
  len  = list.length(sources)
  n    = ref(0)
  fail = source.fail()

  def rec s()
    sn = list.nth(default=list.last(default=fail, sources), sources, n())
    if source.is_ready(sn) or n() >= len - 1 then
      sn
    else
      ref.incr(n)
      s()
    end
  end
  infallible = not source.fallible(list.last(default=fail, sources))
  s = source.dynamic(id=id, infallible=infallible, track_sensitive=true, s)

  first = ref(true)
  def ot(_)
    # Drop the first track
    if first() then first := false else
      ref.incr(n)
    end
  end

  s = source.on_track(s, ot)
  s
end

# Select the first source whose predicate is true in a list. If the second
# argument is a getter, the source will be dynamically created.
# @category Source / Track processing
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

# @docof request.dynamic
def native.request.dynamic(%argsof(request.dynamic), f)
  ignore(available)
  ignore(timeout)

  def f() =
    try
      f()
    catch _ do
      null()
    end
  end

  # Prepared requests
  queue = ref([])

  def get_queue() =
    def get_request(s) =
      s.request
    end
    list.map(get_request, queue())
  end

  def add(r) =
    s = request.once(r)
    if s.resolve() then
      queue := [...queue(), s]
      true
    else
      false
    end
  end

  def set_queue(l) =
    queue := []
    list.iter(fun (r) -> ignore(add(r)), l)
  end

  current = ref(null())

  def get_current() =
    if null.defined(current()) then
      s = null.get(current())
      s.request
    else
      null()
    end
  end

  # Prefetch thread
  def fetch()
    r = f()
    if null.defined(r) then
      r = null.get(r)
      s = request.once(r)
      if s.resolve() then
        log.info("Added request on queue: #{request.uri(r)}.")
        queue := [...queue(), s]
        true
      else
        log.important("Failed to resolve request #{request.uri(r)}.")
        false
      end
    else
      false
    end
  end

  def fill() =
    if list.length(queue()) < prefetch then
      ignore(fetch())
    end
  end

  thread.run(every=retry_delay, fill)

  # Source
  def s()
    if not list.is_empty(queue()) then
      let [s, ...rest] = queue()
      queue := rest
      current := s
      s
    else
      source.fail()
    end
  end
  source.dynamic(id=id, track_sensitive=true, s).{
    fetch=fetch,
    queue=get_queue,
    add=add,
    set_queue=set_queue,
    current=get_current
  }
end
