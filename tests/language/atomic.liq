#!../../liquidsoap ../test.liq

# A one-shot guard: a thread that finds the work already claimed must also find
# it finished. Claiming the flag indivisibly is not enough on its own, which is
# how a half-registered playlist parser table got read.
def once_guard() =
  claimed = ref(false)
  steps = ref(0)
  torn = ref(false)
  callers = ref(0)

  def init() =
    def body() =
      if
        claimed()
      then
        if steps() != 20000 then torn := true end
      else
        claimed := true
        for _ = 1 to 20000 do steps := steps() + 1  end
      end
    end
    atomic(body)
    ref.incr(callers)
    (-1.)
  end

  for _ = 1 to 6 do
    thread.run.recurrent(fast=true, delay=0., on_error=null, init)

  end

  def check() =
    if
      callers() >= 6
    then
      test.equal(torn(), false)
      test.equal(steps(), 20000)
      test.pass()
      (-1.)
    else
      0.
    end
  end
  thread.run.recurrent(fast=true, delay=0., on_error=null, check)
end

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
      once_guard()
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
