log.level.set(4)
n = ref(0)

def f() =
  n := !n + 1
  print(
    "Frame #{!n}"
  )
  if !n > 10 then test.pass() end
end

s = sine()
s.on_frame(synchronous=true, f)
output.dummy(s)
