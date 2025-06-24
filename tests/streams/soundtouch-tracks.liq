# Ensure that soundtouch preserves tracks, see #2467.

n = ref(0)
def ot(_) =
  print(
    "New track!"
  )
  ref.incr(n)
  if
    n() >= 3
  then
    test.pass()
    shutdown()
  end
end
s = chop(every=1., sine())
s = soundtouch(s)
s.on_track(fast=true, ot)
output.dummy(fallible=true, s)
