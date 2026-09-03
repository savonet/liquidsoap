#!../../liquidsoap ../test.liq

# Two writers keep `a` and `b` equal; a reader that ever sees them apart proves
# the sections interleaved. Each task does a batch, so the run is dominated by
# the sections rather than by scheduling them.
def concurrent() =
  a = ref(0)
  b = ref(0)
  batches = ref(0)

  def bump() =
    def step() =
      a := a() + 1
      b := b() + 1
    end
    for _ = 1 to 1000 do atomic(step)  end
    ref.incr(batches)
    0.
  end

  def check() =
    def compare() =
      test.equal(a(), b())
    end
    for _ = 1 to 1000 do atomic(compare)  end
    if
      batches() >= 20
    then
      test.pass()
      (-1.)
    else
      0.
    end
  end

  thread.run.recurrent(fast=true, delay=0., on_error=null, bump)
  thread.run.recurrent(fast=true, delay=0., on_error=null, bump)
  thread.run.recurrent(fast=true, delay=0., on_error=null, check)
end

def f() =
  test.equal(atomic({42}), 42)
  test.equal(atomic(fast=true, {43}), 43)

  # Nesting is a no-op, whichever way the modes are mixed.
  test.equal(atomic({atomic(fast=true, {1})}), 1)
  test.equal(atomic(fast=true, {atomic({2})}), 2)

  # A raising section still releases, otherwise the next one never returns.
  test.raises({atomic({error.raise(error.not_found, "boom")})})
  test.equal(atomic({44}), 44)

  concurrent()
end

test.check(f)
