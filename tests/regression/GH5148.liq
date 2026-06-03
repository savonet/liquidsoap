# Regression for https://github.com/savonet/liquidsoap/discussions/5148
#

def f() =
  iterations = ref(0)

  def next() =
    request.create("../media/@wav[stereo].wav")
  end

  s = request.dynamic(id="dyn", synchronous=true, next)

  s = cross(duration=0.5, fun (a, b) -> sequence([a.source, b.source]), s)

  clock.assign_new(sync="none", [s])
  output.dummy(fallible=true, s)

  # Hammer skip from another thread
  def hammer() =
    s.skip()
    ref.incr(iterations)
    if 1000 < iterations() then test.pass() end
    0.001
  end

  thread.run.recurrent(fast=true, delay=0.5, hammer)
end

test.check(f)
