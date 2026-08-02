# A transition returning `sequence([before, after])` only pulls the incoming
# source once the outgoing one is done. `b` sets a cross duration of a single
# frame and its duration leaves a partial frame, so both sources end up in the
# same frame with exactly as many samples available. The incoming source used to
# be rejected there, dropping its first chunk along with `c`'s metadata, which
# left the stream advertising `b` until the next track boundary.

success = ref(false)

a = sine(duration=4., 440.)
a =
  metadata.map(
    insert_missing=true,
    id="a",
    update=false,
    (fun (_) -> [("source", "a"), ("liq_cross_duration", "2.")]),
    a
  )

b = sine(duration=4.02, 880.)
b =
  metadata.map(
    insert_missing=true,
    id="b",
    update=false,
    (fun (_) -> [("source", "b"), ("liq_cross_duration", "0.")]),
    b
  )

c = sine(duration=4., 440.)
c =
  metadata.map(
    insert_missing=true,
    id="c",
    update=false,
    (fun (_) -> [("source", "c"), ("liq_cross_duration", "2.")]),
    c
  )

s = sequence([a, b, c])

def transition(before, after) =
  sequence([before.source, after.source])
end

s = cross(duration=2., transition, s)

track_count = ref(0)

def on_metadata(m) =
  ref.incr(track_count)

  if track_count() == 1 then if m["source"] != "a" then test.fail() end end

  if track_count() == 2 then if m["source"] != "b" then test.fail() end end

  if
    track_count() == 3
  then
    if m["source"] != "c" then test.fail() end
    success := true
  end
end

s.on_metadata(synchronous=true, on_metadata)
clock.assign_new(sync="none", [s])

def on_stop() =
  if success() then test.pass() else test.fail() end
end

o = output.dummy(fallible=true, s)
o.on_stop(synchronous=true, on_stop)
