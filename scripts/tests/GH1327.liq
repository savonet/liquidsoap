%include "test.liq"

selected = ref []

s1 = blank(duration=0.1)

def m1(_) =
  [("id","s1")]
end

s1 = map_metadata(id="s1",m1,s1)

s2 = blank(duration=0.1)

def m2(_) =
  [("id","s2")]
end

s2 = map_metadata(id="s2",m2,s2)

def f(m) =
  selected := list.add(m["id"],!selected)
end

s = rotate(weights=[2,1],[s1,s2])

s = on_track(f,s)

def m_noise(_) =
  [("id","noise")]
end

noise = map_metadata(id="noise", m_noise, noise())

ready = ref false

s = switch(track_sensitive=false,[({!ready}, s), ({true}, noise)])

def f(_) =
  ready := true
end

s = on_track(f,s)

output.dummy(s)

def on_done () =
  s = list.rev(!selected)
  if list.nth(default="",s,0) == "s1" and 
     list.nth(default="",s,1) == "s1" and
     list.nth(default="",s,2) == "s2" and
     list.nth(default="",s,3) == "s1" and
     list.nth(default="",s,4) == "s1" and
     list.nth(default="",s,5) == "s2" then
    test.pass()
  else
    test.fail()
  end

  shutdown()
  (-1.)
end

add_timeout(1., on_done)
