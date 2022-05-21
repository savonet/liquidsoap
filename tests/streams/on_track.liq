log.level.set(4)

def f(_)
  print("Got track!")
  test.pass()
  shutdown()
end

s = sequence([sine(duration=1.), sine()])
s = source.on_track(s,f)

output.dummy(s)
