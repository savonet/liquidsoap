selected = ref([])

s1 = blank(duration=0.04)

def m1(_) =
  [("id","s1")]
end

s1 = metadata.map(id="s1",m1,s1)

s2 = blank(duration=0.04)

def m2(_) =
  [("id","s2")]
end

s2 = metadata.map(id="s2",m2,s2)

def f(m) =
  selected := list.add(m["id"],!selected)
end

s = rotate(weights=[2,1],[s1,s2])

s.on_track(f)

output.dummy(s)

def on_done () =
  s = list.rev(!selected)
  if list.nth(default="",s,0) == "s1" and
     list.nth(default="",s,1) == "s1" and
     list.nth(default="",s,2) == "s2" and
     list.nth(default="",s,3) == "s1" and
     list.nth(default="",s,4) == "s1" then
    test.pass()
  else
    test.fail()
  end

  shutdown()
end

thread.run(delay=1., on_done)
