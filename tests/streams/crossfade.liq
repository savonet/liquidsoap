video.frame.rate.set(24)

a = sine(duration=20.,440.)
a = metadata.map(update=false, (fun (_) -> [("title","a1")]), a)

b = sine(duration=20.,880.)
b = metadata.map(update=false, (fun (_) -> [("title","b1")]), b)

s = sequence([a,b])

fa = crossfade(smart=true, deduplicate=true, s)

a = sine(duration=20.,440.)
a = metadata.map(update=false, (fun (_) -> [("title","a2")]), a)

b = sine(duration=20.,880.)
b = metadata.map(update=false, (fun (_) -> [("title","b2")]), b)

s = sequence([a,b])

fb = crossfade(smart=true, deduplicate=false, conservative=false, s)

let {track_marks=_, ...tracks} = source.tracks(fa)

s = sequence([source(tracks), fb])

dedup_a = ref(0)
dedup_b = ref(0)
dup_a = ref(0)
dup_b = ref(0)

def check_duplicate(m) =
  if m["title"] == "a1" then
    dedup_a := dedup_a() + 1
  end
  if m["title"] == "b1" then
    dedup_b := dedup_b() + 1
  end
  if m["title"] == "a2" then
    dup_a := dup_a() + 1
  end
  if m["title"] == "b2" then
    dup_b := dup_b() + 1
  end
end

s.on_metadata(check_duplicate)

clock.assign_new(sync="none",[s])

def on_stop () =
  if !dup_a == 2 and !dup_b == 2 and !dedup_a == 1 and !dedup_b == 1 then
    test.pass()
  else
    test.fail()
  end
end

output.dummy(fallible=true, on_stop=on_stop,s)
