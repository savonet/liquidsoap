log.level.set(4)

def f(_) =
  print(
    "Got track!"
  )
  test.pass()
end

s = sequence([sine(duration=1.), sine()])
s.on_track(synchronous=true, f)
output.dummy(s)
