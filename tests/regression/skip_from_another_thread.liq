%include "../test.liq"

# Skips issued from another thread while the stream runs are carried out
# between streaming cycles: a thousand of them in a row neither stop the
# stream nor leave a frame short, and tracks do get skipped.

s =
  fallback(
    [source.dynamic(infallible=true, track_sensitive=true, {sine(duration=5.)})]
  )

skips_wanted = 1000
skips = ref(0)
tracks = ref(0)
frames_after = ref(0)

s.on_track(synchronous=true, fun (_) -> ref.incr(tracks))

def on_frame() =
  if
    skips() == skips_wanted
  then
    ref.incr(frames_after)
    if
      frames_after() == 50
    then
      # Each track would last 5 s if it were not skipped.
      if
        tracks() < 10
      then
        test.fail(
          "only #{tracks()} tracks for #{skips()} skips"
        )
      else
        test.pass()
      end
    end
  end
end

s.on_frame(synchronous=true, on_frame)

def skip() =
  if
    skips() < skips_wanted
  then
    ref.incr(skips)
    s.skip()
    0.001
  else
    (-1.)
  end
end

thread.run.recurrent(skip)

# Not fallible: a short frame fails the clock.
output.dummy(s)
