success = ref(false)

a = sine(duration=5., 440.)
a =
  metadata.map(
    insert_missing=true, id="a", update=false, (fun (_) -> [("source", "a")]), a
  )

b = sine(duration=5., 880.)
b =
  metadata.map(
    insert_missing=true,
    id="b",
    update=false,
    (fun (_) -> [("source", "b"), ("liq_cross_duration", "1.1")]),
    b
  )

c = sine(duration=5., 440.)
c =
  metadata.map(
    insert_missing=true, id="c", update=false, (fun (_) -> [("source", "c")]), c
  )

s = sequence([a, b, c])
s = crossfade(persist_override=true, duration=5., s)

track_count = ref(0)

def on_metadata(m) =
  ref.incr(track_count)

  if track_count() == 1 then if m["source"] != "a" then test.fail() end end

  if
    track_count() == 2
  then
    if m["source"] != "b" then test.fail() end
    if s.start_duration() != 1.1 then test.fail() end
    if s.end_duration() != 1.1 then test.fail() end
  end

  if
    track_count() == 3
  then
    if m["source"] != "c" then test.fail() end
    if s.start_duration() != 1.1 then test.fail() end
    if s.end_duration() != 1.1 then test.fail() end
    success := true
  end
end

s.on_metadata(fast=true, on_metadata)
clock.assign_new(sync="none", [s])

def on_stop() =
  if !success then test.pass() else test.fail() end
end

output.dummy(fallible=true, on_stop=on_stop, s)
