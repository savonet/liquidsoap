# Select the first source whose predicate is true in a list.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~override Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
# @param ~replay_metadata Replay the last metadata of a child when switching to it in the middle of a track.
# @param ~track_sensitive Re-select only on end of tracks.
# @param ~transition_length Maximun transition duration.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
# @param sources Sources with the predicate telling when they can be played.
def switch(~id="", ~override="liq_transition_length", ~replay_metadata=true, ~track_sensitive=bool_getter(true), ~transition_length=5., ~transitions=[], sources)
  def s()
    def activated(ps)
      let (p,s) = ps
      p() and source.is_ready(s)
    end
    ps = list.find(default=({true},fail()), activated, sources)
    snd(ps)
  end
  source.dynamic(track_sensitive=track_sensitive, s)
end

# At the beginning of each track, select the first ready child.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~override Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
# @param ~replay_metadata Replay the last metadata of a child when switching to it in the middle of a track.
# @param ~track_sensitive Re-select only on end of tracks.
# @param ~transition_length Maximun transition duration.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
def fallback(~id="", ~override="liq_transition_length", ~replay_metadata=true,
           ~track_sensitive=true, ~transition_length=5., ~transitions=[],
           ~weights=[], sources) =
  def s()
    list.find(default=fail(), source.is_ready, sources)
  end

  infallible = not(list.for_all(source.fallible, sources))
  source.dynamic(infallible=infallible, track_sensitive=track_sensitive, s)
end

# Rotate between sources.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~override Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
# @param ~replay_metadata Replay the last metadata of a child when switching to it in the middle of a track.
# @param ~transition_length Maximun transition duration.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
# @param ~weights Weights of the children (padded with 1), defining for each child how many tracks are played from it per round, if that many are actually available.
def rotate(~id="", ~override="liq_transition_length", ~replay_metadata=true,
           ~transition_length=5., ~transitions=[],
           ~weights=[], sources) =
  weights = list.map(to_int_getter, weights)
  picked_index = ref -1
  tracks_played = ref 0
  default_weight = fun () -> 1

  def pick() =
    tracks_played := 0

    if list.exists(source.is_ready, sources) then
      def rec f(index) =
        s = list.nth(default=fail(), sources, index)
        if source.is_ready(s) then
          picked_index := index
        else
          f((index + 1) mod list.length(sources))
        end
      end
      f((!picked_index+1) mod list.length(sources))
    else
      picked_index := -1
    end
  end
  pick()

  fail = fail()
  def s()
    list.nth(default=fail, sources, !picked_index)
  end

  infallible = not(list.for_all(source.fallible, sources))
  s = source.dynamic(infallible=infallible, track_sensitive=true, s)

  first = ref(true)
  def ot(_)
    if !first then
      first := false
    else
      tracks_played := !tracks_played + 1
      weight = list.nth(default=default_weight, weights, !picked_index)
      if !tracks_played >= weight() then pick() end
    end
  end
  on_track(ot, s)
end

# TODO: this is ugly, use better naming conventions...
random_int = random.int
random_float = random.float
random_bool = random.bool

# At the beginning of every track, select a random ready child.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~override Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
# @param ~replay_metadata Replay the last metadata of a child when switching to it in the middle of a track.
# @param ~transition_length Maximun transition duration.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
# @param ~weights Weights of the children (padded with 1), defining for each child the probability that it is selected.
def random(~id="", ~override="liq_transition_length", ~replay_metadata=true,
           ~transition_length=5., ~transitions=[],
           ~weights=[], sources) =
  weights = list.map(to_int_getter, weights)
  default_weight = fun () -> 1
  next_index = ref -1

  def pick() =
    def available_weighted_sources(cur, s) =
      let (index, current_weight, indexes) = cur
      weight = list.nth(default=default_weight, weights, index)

      let (current_weight, indexes) =
        if source.is_ready(s) then
          weight = weight()
          indexes = list.add((current_weight, current_weight+weight, index), indexes)
          (current_weight + weight, indexes)
        else
          (current_weight, indexes)
        end
    
      (index+1, current_weight, indexes)
    end

    let (_, total_weight, weighted_indexes)  =
      list.fold(available_weighted_sources, (0, 0, []), sources)

    picked_weight =
      if total_weight > 0 then
        random.int(min=0, max=total_weight)
      else
        0
      end
      
    def pick_source(picked_index, el) =
      let (lower_bound, upper_bound, index) = el

      if lower_bound <= picked_weight and picked_weight < upper_bound then
        index
      else
         picked_index
      end
    end
 
    next_index := list.fold(pick_source, -1, weighted_indexes)
 end

  def add_condition(index, s) =
    def f(_) =
      next_index := -1
    end 

    s = on_track(f, s)

    def cond() =
      if !next_index == -1 then pick() end
      !next_index == index  
    end
    
    (cond, s)
  end

  s = switch(override=override, replay_metadata=replay_metadata, track_sensitive=true,
             transition_length=transition_length, transitions=transitions,
             list.mapi(add_condition, sources))

  fallback(id=id, track_sensitive=true, list.add(s, sources))
end

# Play only one track of every successive source, except for the last one which
# is played as much as available.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~merge Merge tracks when advancing from one source to the next one. This will NOT merge consecutive tracks from the last source; see merge_tracks() if you need that too.
# @param sources List of sources to play tracks from.
def sequence(~id="sequence", ~merge=false, sources)
  if merge then log.severe(label=id, "merge option of sequence operator is not supported anymore (for now). Use merge_tracks if you need to merge tracks.") end
  len = list.length(sources)
  n = ref(0)
  fail = fail()

  def incr() = n := !n + 1 end

  def rec s()
    sn = list.nth(default=list.last(default=fail, sources), sources, !n)
    if source.is_ready(sn) or !n >= len - 1 then
      sn
    else
      incr()
      s()
    end
  end
  infallible = not(source.fallible(list.last(default=fail, sources)))
  s = source.dynamic(infallible=infallible, track_sensitive=true, s)

  first = ref(true)
  def ot(_)
    if !first then
      # Drop the first track
      first := false
    else
      incr ()
    end
  end

  s = on_track(ot, s)
  s
end

let random.int = random_int
let random.float = random_float
let random.bool = random_bool
