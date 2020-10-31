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
  def add_condition(s) =
    ({true}, s)
  end

  switch(id=id, override=override, replay_metadata=replay_metadata, transitions=transitions,
         track_sensitive=track_sensitive, transition_length=transition_length,
         list.map(add_condition, sources))
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
  picked_index = ref(-1)
  tracks_played = ref(0)
  default_weight = fun () -> 1

  def pick() =
    tracks_played := 0

    if list.exists(fun (s) -> s.is_ready(), sources) then
      def rec f(index) =
        s = list.nth(default=fail(), sources, index)
        if s.is_ready() then
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

  def add_condition(index, s) =
    weight = list.nth(default=default_weight, weights, index)

    def f(_) =
      tracks_played := !tracks_played + 1
    end

    s.on_track(f)

    def cond() =
      if !picked_index == -1 then pick() end

      if !picked_index == index then
        if !tracks_played < weight () then
          true
        else
           pick()
           !picked_index == index
        end
      else
        false
      end
    end

    (cond, s)
  end

  s = switch(override=override, replay_metadata=replay_metadata, track_sensitive=true,
             transition_length=transition_length, transitions=transitions,
             list.mapi(add_condition, sources))

  fallback(id=id, track_sensitive=true, list.add(s, sources))
end

# At the beginning of every track, select a random ready child.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~override Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
# @param ~replay_metadata Replay the last metadata of a child when switching to it in the middle of a track.
# @param ~transition_length Maximun transition duration.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
# @param ~weights Weights of the children (padded with 1), defining for each child the probability that it is selected.
def replaces random(~id="", ~override="liq_transition_length", ~replay_metadata=true,
           ~transition_length=5., ~transitions=[],
           ~weights=[], sources) =
  weights = list.map(to_int_getter, weights)
  default_weight = fun () -> 1
  next_index = ref(-1)

  def pick() =
    def available_weighted_sources(cur, s) =
      let (index, current_weight, indexes) = cur
      weight = list.nth(default=default_weight, weights, index)

      let (current_weight, indexes) =
        if s.is_ready() then
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

    s.on_track(f)

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
