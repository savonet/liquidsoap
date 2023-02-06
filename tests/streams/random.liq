# We want:
# Track selection 1 -> s1 ready, s2 not ready -> switch to s1
# Track selection 2 -> s1 not ready, s2 ready -> switch to s2
# Track selection 3 -> s1 ready, s2 with weight 0 -> switch to s1

selected = ref([])

s1 = blank(duration=0.04)

def m1(_) =
  [("id","s1")]
end

s1 = metadata.map(m1,s1)

s1_ready = ref(true)

s1 = switch(id="s1",[(s1_ready,s1)])

s1_weight = ref(1)

s2 = blank(duration=0.04)

def m2(_) =
  [("id","s2")]
end

s2 = metadata.map(id="s2",m2,s2)

s2_ready = ref(false)

s2 = switch(id="s2",[(s2_ready,s2)])

s2_weight = ref(1)

round = ref(1)

def f(m) =
  if round() == 1 then
    s1_ready := false
    s2_ready := true
  elsif round() == 2 then
    s1_ready := true
    s2_ready := true
    s2_weight := 0
  else
    s1_ready := true
    s1_weight := 2
    s2_ready := true
    s2_weight := 1
  end
  round := round() + 1
  selected := list.cons(m["id"],selected())
end

s = random(weights=[s1_weight, s2_weight], [s1,s2])

s.on_track(f)

output.dummy(fallible=true, s)

def on_done () =
  s = list.rev(selected())
  if list.nth(default="",s,0) == "s1" and
     list.nth(default="",s,1) == "s2" and
     list.nth(default="",s,2) == "s1" then
    test.pass()
  else
    test.fail()
  end

  shutdown()
end

thread.run(delay=1., on_done)
