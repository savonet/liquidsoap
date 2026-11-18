%include "../test.liq"

# Tracks far shorter than a frame go through a switch one by one: each is
# announced, none is merged with its neighbours, and several fit in a frame.

tracks_wanted = 500
handed_out = ref(0)

def next() =
  ref.incr(handed_out)
  sine(duration=0.002)
end

s = source.dynamic(infallible=true, track_sensitive=true, next)
s = fallback([s])

tracks = ref(0)

def on_track(_) =
  ref.incr(tracks)
  if
    tracks() == tracks_wanted
  then
    elapsed = source.time(s)
    if
      handed_out() != tracks()
    then
      test.fail(
        "#{handed_out()} sources played, #{tracks()} tracks announced"
      )
    elsif
      float(tracks_wanted) * settings.frame.duration() / 2. < elapsed
    then
      test.fail(
        "#{tracks()} tracks took #{elapsed}s: fewer than two per frame"
      )
    else
      test.pass()
    end
  end
end

s.on_track(synchronous=true, on_track)

clock.assign_new(sync="none", [s])
output.dummy(s)
