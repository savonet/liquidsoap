%include "../test.liq"

# Two properties of `cross` seen from outside.
#
# Remaining time is the time to the next mark `cross` will put: it goes down
# by what was delivered, cycle after cycle, and is zero where the track of
# the next crossing begins.
#
# Put to sleep and woken, in a track and in a transition, it starts again
# from nothing: a track begins and the stream goes on.

frame = settings.frame.duration()
pending = ref(2)

def finished() =
  pending := pending() - 1
  if pending() == 0 then test.pass() end
end

def tagged(title, duration) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    sine(duration=duration)
  )
end

def mix(ending, starting) =
  add(normalize=false, [ending.source, starting.source])
end

# Remaining time: tracks of 3 s crossed over 1 s begin every 2 s.
child = sequence([tagged("a", 3.), tagged("b", 3.), tagged("c", 3.)])
s = cross(duration=1., deduplicate=false, mix, child)

track_starts = ref([])
last_remaining = ref(null)
checked = ref(0)

def on_frame() =
  now = source.time(s)
  remaining = s.remaining()

  # Between the first and the last mark, the next one is known whenever
  # the child knows its own.
  if
    (list.length(track_starts()) == 1 or list.length(track_starts()) == 2)
    and child.remaining() != infinity
  then
    next_mark = 2. * float(list.length(track_starts()))
    ref.incr(checked)
    if
      abs(remaining - (next_mark - now)) > 2. * frame
    then
      test.fail(
        "at #{now}s: #{remaining}s remaining, the next track begins at #{
          next_mark
        }s"
      )
    end

    if
      null.defined(last_remaining())
      and null.get(last_remaining()) < remaining - frame / 2.
      and list.length(track_starts()) == 1
    then
      test.fail(
        "at #{now}s: remaining went up to #{remaining}s"
      )
    end
  end

  last_remaining := remaining
end

s.on_frame(synchronous=true, on_frame)
s.on_track(
  synchronous=true,
  fun (_) ->
    begin
      track_starts := [...track_starts(), source.time(s)]
      last_remaining := null
    end
)

clock.assign_new(sync="none", [s])
o = output.dummy(fallible=true, s)
o.on_stop(
  synchronous=true,
  fun () ->
    begin
      if
        checked() < 100
      then
        test.fail(
          "remaining time checked #{checked()} times"
        )
      end
      finished()
    end
)

# Sleep and wake: a crossed source is dropped by a `source.dynamic` and taken
# again, once while a track plays and once during a transition.
def numbered() =
  count = ref(0)
  def next() =
    ref.incr(count)
    title = "t#{count()}"
    tagged(title, 3.)
  end

  source.dynamic(self_sync=false, track_sensitive=true, next)
end

crossed = cross(duration=1., deduplicate=false, mix, numbered())
other = (sine(880.) : source)
use_crossed = ref(true)
d =
  source.dynamic(
    track_sensitive=false,
    {use_crossed() ? (crossed : source) : other}
  )

frames = ref(0)
crossed_tracks = ref(0)
tracks_after_wake = ref(0)
woken = ref(0)
crossed.on_track(
  synchronous=true,
  fun (_) ->
    begin
      ref.incr(crossed_tracks)
      if 0 < woken() then ref.incr(tracks_after_wake) end
    end
)

crossed.on_wake_up(
  synchronous=true,
  fun () -> if 0 < frames() then ref.incr(woken) end
)

def drive() =
  ref.incr(frames)

  # 1 s in: a track is playing. Away for half a second.
  if frames() == 50 then use_crossed := false end
  if frames() == 75 then use_crossed := true end

  # A crossing starts 2 s after `cross` starts again, and lasts 1 s: leave in
  # the middle of it.
  if frames() == 75 + 125 then use_crossed := false end
  if frames() == 75 + 150 then use_crossed := true end

  if
    frames() == 75 + 150 + 300
  then
    if
      woken() != 2
    then
      test.fail(
        "sleep and wake: woken #{woken()} times"
      )
    end

    # After the second wake: its first track, then crossings every 2 s.
    if
      tracks_after_wake() < 4
    then
      test.fail(
        "sleep and wake: #{tracks_after_wake()} tracks since the first wake"
      )
    end

    finished()
  end
end

d.on_frame(synchronous=true, drive)
clock.assign_new(sync="none", [d])
output.dummy(fallible=true, d)
