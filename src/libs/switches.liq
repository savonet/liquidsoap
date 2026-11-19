# Type the children of a switch wrapper with their optional composition
# methods and fill in the defaults for `weight` and `track_sensitive`. Shared by the wrappers below so
# that the method list is written down once.
# @category Source / Track processing
# @flag hidden
def source.composition.children(sources) =
  sources =
    (sources :
      [
        source.{
          on_select?: (
            {ending: source?, replay_metadata: bool, starting: source}
          )->source,
          on_leave?: ({source: source, preempted: bool})->unit,
          replay_metadata?: getter(bool),
          single?: bool,
          track_sensitive?: getter(bool),
          weight?: getter(int)
        }
      ]
    )

  # These policies count tracks, so a child is always held until its track
  # ends. Left to its profile, a child that is live when selected (a playlist
  # still on its silence of last resort) would be cut at once, start a track
  # by being entered, and be cut again.
  list.map(
    fun (s) ->
      s.{weight = s?.weight ?? getter(1), track_sensitive = getter(true)},
    sources
  )
end

# At the beginning of each track, select the first ready child.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
def fallback(~id=null, sources) =
  switch(id=id, list.map(fun (s) -> ({true}, s), sources))
end

# Rotate between sources: play `weight` tracks of each source in turn. A source
# is always played until the end of its track: `track_sensitive` is not read on
# the sources given here, since a turn is counted in tracks. To let a source
# cut into the rotation, put it in a `fallback` or `switch` above it.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param sources Sources to rotate between. Each can carry a `weight`, the number of tracks it gets per turn (default: `1`).
def rotate(~id=null, sources) =
  sources = source.composition.children(sources)
  count = list.length(sources)
  indexes = list.init(count, fun (index) -> index)

  # The child whose turn it is, and how many tracks of that turn were played.
  # Both change when a track is played, never when a predicate is read.
  cursor = ref(-1)
  played = ref(0)

  def weight(index) =
    getter.get(list.nth(sources, index).weight)
  end

  def available(index) =
    source.is_ready(list.nth(sources, index))
  end

  # The child to play from here.
  def target() =
    if
      cursor() != -1 and played() < weight(cursor()) and available(cursor())
    then
      cursor()
    else
      # From the child after the cursor, wrapping around, the cursor last.
      turns =
        list.map(fun (offset) -> (cursor() + 1 + offset) mod count, indexes)
      next_turn =
        list.find(
          default=-1,
          fun (index) -> 1 <= weight(index) and available(index),
          turns
        )

      if
        next_turn != -1
      then
        next_turn
      else
        list.find(default=-1, available, indexes)
      end
    end
  end

  s = switch(list.mapi(fun (index, s) -> ({target() == index}, s), sources))

  def count_track(_) =
    selected = s.selected()
    if
      null.defined(selected)
    then
      index = list.index(fun (c) -> (c : source) == null.get(selected), sources)
      if
        index == cursor()
      then
        ref.incr(played)
      elsif
        index < count
      then
        cursor := index
        played := 1
      end
    end
  end

  s.on_track(synchronous=true, count_track)

  def replaces s =
    rotation = s
    s = fallback(id=id, rotation::sources)
    source.set_id(rotation, "#{source.id(s)}.rotate")
    s
  end

  s
end

# At the beginning of every track, select a random ready child. A source is
# always played until the end of its track: `track_sensitive` is not read on the
# sources given here, since a new one is drawn per track.
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param sources Sources to choose from. Each can carry a `weight`, its relative probability (default: `1`).
def replaces random(~id=null, sources) =
  sources = source.composition.children(sources)

  # Drawn again when a track is played, never when a predicate is read.
  draw = ref(random.float())

  # The child to play from here: the available children of positive weight
  # laid end to end, each as wide as its weight.
  def target() =
    def add_interval(laid_out, s) =
      let (index, total, intervals) = laid_out
      weight = getter.get(s.weight)
      if
        source.is_ready((s : source.{ weight: getter(int) })) and 1 <= weight
      then
        (index + 1, total + weight, (total, total + weight, index)::intervals)
      else
        (index + 1, total, intervals)
      end
    end

    let (_, total, intervals) = list.fold(add_interval, (0, 0, []), sources)
    picked = draw() * float(total)
    interval =
      list.find(
        default=(0, 0, -1),
        fun (interval) ->
          begin
            let (lower, upper, _) = interval
            float(lower) <= picked and picked < float(upper)
          end,
        intervals
      )

    let (_, _, index) = interval
    if index != -1 then index else list.index(source.is_ready, sources) end
  end

  s = switch(list.mapi(fun (index, s) -> ({target() == index}, s), sources))
  s.on_track(synchronous=true, fun (_) -> draw := random.float())

  def replaces s =
    selection = s
    s = fallback(id=id, selection::sources)
    source.set_id(selection, "#{source.id(s)}.random")
    s
  end

  s
end
