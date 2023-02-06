video.frame.rate.set(24)

a = sine(duration=5.,440.)
a = metadata.map(update=false, (fun (_) -> [("title","a")]), a)

b = sine(duration=5.,880.)
b = metadata.map(update=false, (fun (_) -> [("title","b")]), b)

s = sequence([a,b])

def t(a, b)
  log.important("Crossing!")
  log.important("Levels: #{a.db_level} / #{b.db_level}")
  log.important("Metadata: #{a.metadata} / #{b.metadata}")
  sequence([a.source, b.source])
end

s = cross(duration=3., width=1., t, s)

seen_a = ref(false)
seen_b = ref(false)

def check_duplicate(m) =
  if m["title"] == "a" then
    if seen_a() then test.fail() else seen_a := true end
  end
  if m["title"] == "b" then
    if seen_b() then test.fail() else seen_b := true end
  end
end

s.on_metadata(check_duplicate)

clock.assign_new(sync="none",[s])

def on_stop () =
  if seen_a() and seen_b() then
    test.pass()
  else
    test.fail()
  end
end

output.dummy(fallible=true, on_stop=on_stop,s)
