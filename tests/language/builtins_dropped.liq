#!../../liquidsoap

%include "../test.liq"

dropped_fired = ref(false)

# Nothing in the script holds `random.bool` once it is loaded.
runtime.gc.finalise.last({dropped_fired := true}, random.bool)

def f() =
  deadline = time() + 20.
  rounds = ref(0)

  def check() =
    runtime.gc.full_major()
    ref.incr(rounds)
    if
      dropped_fired() and rounds() >= 3
    then
      # Collecting the builtins must not close the standard descriptors.
      test.equal(file.exists("/dev/fd/0"), true)
      test.equal(file.exists("/dev/fd/2"), true)
      print(
        "standard output still open"
      )

      # Predicates look up their builtins when they run.
      p = time.predicate("0h-24h")
      test.equal(p(), true)
      test.pass()
      (-1.)
    elsif
      time() > deadline
    then
      test.fail(
        "Unused builtins not collected after #{rounds()} rounds"
      )
      (-1.)
    else
      0.5
    end
  end

  # A finaliser runs on the domain that registered it.
  thread.run.recurrent(domain=runtime.domain(), delay=0., check)
end

test.check(f)
