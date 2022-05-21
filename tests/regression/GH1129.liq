# We want:
# Track selection 1 -> s1 ready, s2 not ready, s3 ready -> switch to s3, s2 gets ready
# Track selection 2 -> s1 ready, s2 ready, s3 ready -> switch to s1
# Bug appears when it switches to s3 again on step 2

selected = ref([])

s1 = blank(duration=0.04)

def m1(_) =
  [("id","s1")]
end

s1 = metadata.map(id="s1",m1,s1)

s2_ready = ref(false)

s2 = switch(id="s2",[({!s2_ready},blank(duration=0.04))])

def m2(_) =
  [("id","s2")]
end

s2 = metadata.map(id="s2",m2,s2)

s3 = blank(id="s3",duration=0.04)

def m3(_) =
  [("id","s3")]
end

s3 = metadata.map(id="s3",m3,s3)

def f(m) =
  s2_ready := !s2_ready or m["id"] == "s3"
  selected := list.cons(m["id"],!selected)
end

s = rotate([s1,s2,s3])

s.on_track(f)

output.dummy(s)

def on_done () =
  s = list.rev(!selected)
  if list.nth(default="",s,0) == "s1" and
     list.nth(default="",s,1) == "s3" and
     list.nth(default="",s,2) == "s1" and
     list.nth(default="",s,3) == "s2" and
     list.nth(default="",s,4) == "s3" then
    test.pass()
  else
    test.fail()
  end

  shutdown()
end

thread.run(delay=1., on_done)
