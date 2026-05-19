# At the beginning of each track, select the first ready child.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
def fallback(~id=null, sources) =
  switch(id=id, list.map(fun (s) -> ({true}, s), sources))
end

# Rotate between sources.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
def rotate(~id=null, sources) =
  sources =
    (sources :
      [
        source.{
          on_select?: (
            {ending: source?, replay_metadata: bool, starting: source}
          )->source,
          on_leave?: ({source: source, track_sensitive: bool})->unit,
          single?: bool,
          track_sensitive?: getter(bool),
          weight?: getter(int)
        }
      ]
    )

  sources = list.map(fun (s) -> s.{weight=s?.weight ?? getter(1)}, sources)
  failed = (source.fail() : source)

  # Currently selected index
  picked_index = ref(-1)

  # Number of tracks played per selected source
  # source IDs can change between calls..
  tracks_played =
    list.map(fun (s) -> ((fun () -> source.id(s)), ref(0)), sources)

  tracks_played =
    fun () ->
      list.map(
        fun (x) ->
          begin
            label_fn = fst(x)
            (label_fn(), snd(x))
          end,
        tracks_played
      )

  # Find index of next source to play, i.e. first ready source after currently
  # selected one.
  def pick() =
    list.iter((fun (el) -> snd(el) := 0), tracks_played())
    if
      list.exists(source.is_ready, sources)
    then
      def rec f(index) =
        s = list.nth(default=failed, sources, index)
        if
          source.is_ready(s)
        then
          picked_index := index
        else
          f((index + 1) mod list.length(sources))
        end
      end

      f((picked_index() + 1) mod list.length(sources))
    else
      picked_index := -1
    end
  end

  # Add condition to i-th source.
  def add_condition(index, s) =
    def cond() =
      if picked_index() == -1 then pick() end
      if
        picked_index() != -1
      then
        picked_source = list.nth(sources, picked_index())
        fn = list.assoc(source.id(picked_source), tracks_played())
        if getter.get(picked_source.weight) <= fn() then pick() end
      end

      picked_index() == index
    end

    (cond, s)
  end

  s = switch(list.mapi(add_condition, sources))

  def f(_) =
    if
      null.defined(s.selected())
    then
      selected_id = source.id(null.get(s.selected()))
      if
        list.assoc.mem(selected_id, tracks_played())
      then
        played = list.assoc(selected_id, tracks_played())
        ref.incr(played)
      end
    end
  end

  s.on_track(synchronous=true, f)

  def replaces s =
    fallback(id=id, s::sources)
  end

  s
end

# At the beginning of every track, select a random ready child.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
def replaces random(~id=null, sources) =
  sources =
    (sources :
      [
        source.{
          on_select?: (
            {ending: source?, replay_metadata: bool, starting: source}
          )->source,
          on_leave?: ({source: source, track_sensitive: bool})->unit,
          single?: bool,
          track_sensitive?: getter(bool),
          weight?: getter(int)
        }
      ]
    )

  sources = list.map(fun (s) -> s.{weight=s?.weight ?? getter(1)}, sources)
  next_index = ref(-1)

  def pick() =
    def available_weighted_sources(cur, s) =
      let (index, current_weight, indexes) = cur
      let (current_weight, indexes) =
        if
          source.is_ready((s : source.{ weight: getter(int) }))
        then
          weight = getter.get(s.weight)
          indexes = (current_weight, current_weight + weight, index)::indexes
          (current_weight + weight, indexes)
        else
          (current_weight, indexes)
        end

      (index + 1, current_weight, indexes)
    end

    let (_, total_weight, weighted_indexes) =
      list.fold(available_weighted_sources, (0, 0, []), sources)

    picked_weight =
      if total_weight > 0 then random.int(min=0, max=total_weight) else 0 end

    def pick_source(picked_index, el) =
      let (lower_bound, upper_bound, index) = el
      if
        lower_bound <= picked_weight and picked_weight < upper_bound
      then
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

  s = switch(list.mapi(add_condition, sources))

  def f(_) =
    next_index := -1
  end

  s.on_track(synchronous=true, f)

  def replaces s =
    fallback(id=id, s::sources)
  end

  s
end
