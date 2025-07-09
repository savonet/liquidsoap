video.frame.rate.set(24)

a = sine(duration=20., 440.)
a =
  metadata.map(
    insert_missing=true, update=false, (fun (_) -> [("title", "a")]), a
  )

b = sine(duration=20., 880.)
b =
  metadata.map(
    insert_missing=true, update=false, (fun (_) -> [("title", "b")]), b
  )

s = sequence([a, b])

s = crossfade(s)

dup_a = ref(0)
dup_b = ref(0)

def check_duplicate(m) =
  if m["title"] == "a" then dup_a := dup_a() + 1 end
  if m["title"] == "b" then dup_b := dup_b() + 1 end
end

s.on_metadata(synchronous=true, check_duplicate)
clock.assign_new(sync="none", [s])

def on_stop() =
  if dup_a() == 1 and dup_b() == 1 then test.pass() else () end
end

o = output.dummy(fallible=true, s)
o.on_stop(on_stop)
