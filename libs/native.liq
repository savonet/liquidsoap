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
  fail = (fail():source)

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
def native.switch(~id="", ~track_sensitive=true, sources)
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
def native.request.dynamic(~id="", ~prefetch=1, ~polling_delay=1., ~timeout=20., f)
  # Prepared requests
  queue = ref([])
  # Filling thread
  def filler()
    if list.length(!queue) < prefetch then
      r = f()
      if null.defined(r) then
        r = null.get(r)
        s = request.once(r)
        if s.resolve() then
          log.info("Added request on queue: #{request.uri(r)}.")
          queue := list.append(!queue, [s])
        else
          log.important("Failed to resolve request #{request.uri(r)}.")
        end
      end
    end
  end
  thread.run(every=polling_delay, filler)
  # Source
  def s()
    if not list.is_empty(!queue) then
      s = list.hd(!queue)
      queue := list.tl(!queue)
      s
    else
      fail()
    end
  end
  source.dynamic(id=id, track_sensitive=true, s)
end

# Play requests dynamically created by a given function.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~polling_delay How often the queue should be checkelid and filled in seconds.
# @param ~prefetch Number of tracks to prepare in advance.
# @param ~timeout Timeout for resolving requests in seconds.
# @param f Function returning the list of next requests.
def native.request.dynamic.list(~id="", ~prefetch=1, ~polling_delay=1., ~timeout=20., f)
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

# Volume normalization function.
# @category Source / Sound Processing
# @param ~id Force the value of the source ID.
# @param ~gain_max Maximal gain value (dB).
# @param ~gain_min Minimal gain value (dB).
# @param ~k_down Coefficient when the power must go down (from 0, the higher, the fastest).
# @param ~k_up Coefficient when the power must go up (from 0, the higher, the fastest).
# @param ~target Desired RMS (dB).
# @param ~threshold  Minimal RMS for activaing gain control (dB).
# @param ~window Duration of the window used to compute the current RMS power (second).
# @param s Source to normalize.
# @method gain Current amplification coefficient.
def native.normalize(~id="", ~target=getter(-13.), ~k_up=getter(0.1), ~k_down=getter(5.), ~gain_min=-9., ~gain_max=9., ~window=0.1, ~threshold=getter(-40.), ~track_sensitive=true, s)
  s = rms(duration=window, s)
  v = ref(1.)
  frame = frame.duration()
  gain_min = lin_of_dB(gain_min)
  gain_max = lin_of_dB(gain_max)
  def update()
    target    = lin_of_dB(getter.get(target))
    threshold = lin_of_dB(getter.get(threshold))
    rms       = s.rms()
    if rms >= threshold then
      if rms <= target then
        k_up = getter.get(k_up) * frame
        v := !v + k_up * ((target / rms) - !v)
      else
        k_down = getter.get(k_down) * frame
        v := !v + k_down * ((target / rms) - !v)
      end
      v := max(gain_min, min(gain_max, !v))
    end
  end
  s = source.on_frame(s, update)
  if track_sensitive then source.on_track(s, fun (_) -> v := 1.) end
  amplify({!v}, s). { gain = fun() -> !v }
end
