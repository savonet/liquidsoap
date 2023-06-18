# At the beginning of each track, select the first ready child.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param ~override Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
# @param ~replay_metadata Replay the last metadata of a child when switching to it in the middle of a track.
# @param ~track_sensitive Re-select only on end of tracks.
# @param ~transition_length Maximum transition duration.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
def fallback(~id=null(), ~override="liq_transition_length", ~replay_metadata=true,
           ~track_sensitive=true, ~transition_length=5., ~transitions=[],
           sources) =
  def add_condition(s) =
    ({true}, s)
  end

  switch(id=id, override=override, replay_metadata=replay_metadata, transitions=transitions,
         track_sensitive=track_sensitive, transition_length=transition_length,
         list.map(add_condition, sources))
end

# Rotate between sources.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param ~override Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
# @param ~replay_metadata Replay the last metadata of a child when switching to it in the middle of a track.
# @param ~transition_length Maximum transition duration.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
# @param ~weights Weights of the children (padded with 1), defining for each child how many tracks are played from it per round, if that many are actually available.
def rotate(~id=null(), ~override="liq_transition_length", ~replay_metadata=true,
           ~transition_length=5., ~transitions=[],
           ~weights=[], sources) =
  weights = list.map(getter.function, weights)
  default_weight = {1}
  failed = (source.fail():source)

  # Currently selected index
  picked_index = ref(-1)

  # Number of tracks played per selected source
  # source IDs can change between calls..
  tracks_played = list.map(fun (s) -> ((fun () -> source.id(s)), ref(0)), sources)
  tracks_played = fun () -> list.map(fun (x) -> begin
    label_fn = fst(x)
    (label_fn(), snd(x))
  end, tracks_played)

  # Find index of next source to play, i.e. first ready source after currently
  # selected one.
  def pick() =
    list.iter((fun (el) -> snd(el) := 0), tracks_played())

    if list.exists(source.is_ready, sources) then
      def rec f(index) =
        s = list.nth(default=failed, sources, index)
        if source.is_ready(s) then
          picked_index := index
        else
          f((index + 1) mod list.length(sources))
        end
      end
      f((picked_index()+1) mod list.length(sources))
    else
      picked_index := -1
    end
  end

  # Add condition to i-th source.
  def add_condition(index, s) =
    def cond() =
      if picked_index() == -1 then pick() end

      if picked_index() != -1 then
        picked_weight = list.nth(default=default_weight, weights, picked_index())
        picked_source = list.nth(sources, picked_index())
        fn = list.assoc(source.id(picked_source), tracks_played())
        if picked_weight () <= fn() then
          pick()
        end
      end

      picked_index() == index
    end

    (cond, s)
  end

  s = switch(override=override, replay_metadata=replay_metadata, track_sensitive=true,
             transition_length=transition_length, transitions=transitions,
             list.mapi(add_condition, sources))

  def f(_) =
    if null.defined(s.selected()) then
      selected_id = source.id(null.get(s.selected()))
      if list.assoc.mem(selected_id, tracks_played()) then
        played = list.assoc(selected_id, tracks_played())
        ref.incr(played)
      end
    end
  end

  s = source.on_track(s, f)

  def replaces s =
    fallback(id=id, track_sensitive=true, s::sources)
  end

  s
end

# At the beginning of every track, select a random ready child.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param ~override Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
# @param ~replay_metadata Replay the last metadata of a child when switching to it in the middle of a track.
# @param ~transition_length Maximum transition duration.
# @param ~transitions Transition functions, padded with `fun (x,y) -> y` functions.
# @param ~weights Weights of the children (padded with 1), defining for each child the probability that it is selected.
def replaces random(~id=null(), ~override="liq_transition_length", ~replay_metadata=true,
           ~transition_length=5., ~transitions=[],
           ~weights=[], sources) =
  weights = list.map(getter.function, weights)
  default_weight = fun () -> 1
  next_index = ref(-1)

  def pick() =
    def available_weighted_sources(cur, s) =
      let (index, current_weight, indexes) = cur
      weight = list.nth(default=default_weight, weights, index)

      let (current_weight, indexes) =
        if source.is_ready(s) then
          weight = weight()
          indexes = (current_weight, current_weight+weight, index) :: indexes
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
    def cond() =
      if next_index() == -1 then pick() end
      next_index() == index
    end

    (cond, s)
  end

  s = switch(override=override, replay_metadata=replay_metadata, track_sensitive=true,
             transition_length=transition_length, transitions=transitions,
             list.mapi(add_condition, sources))

  def f(_) =
    next_index := -1
  end

  s = source.on_track(s, f)

  def replaces s =
    fallback(id=id, track_sensitive=true, s::sources)
  end

  s
end
