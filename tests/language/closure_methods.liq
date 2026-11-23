#!../../liquidsoap

%include "../test.liq"

unused_fired = ref(false)
nested_fired = ref(false)
whole_fired = ref(false)

# The closure only reaches `r.kept`: `r.unused` must not be kept alive.
def capture_method() =
  unused = fun () -> ()
  runtime.gc.finalise.last({unused_fired := true}, unused)
  r = {kept = 1, unused = unused}
  fun () -> r.kept
end

def capture_nested_method() =
  unused = fun () -> ()
  runtime.gc.finalise.last({nested_fired := true}, unused)
  r = {a = {kept = 2, unused = unused}}
  fun () -> r.a.kept
end

# The closure returns `r` itself, with all its methods.
def capture_whole() =
  watched = fun () -> ()
  runtime.gc.finalise.last({whole_fired := true}, watched)
  r = {kept = 3, watched = watched}
  fun () -> r
end

def f() =
  method = capture_method()
  nested = capture_nested_method()
  whole = capture_whole()
  deadline = time() + 20.
  rounds = ref(0)

  def check() =
    runtime.gc.full_major()
    ref.incr(rounds)
    if
      unused_fired() and nested_fired() and rounds() >= 3
    then
      test.equal(whole_fired(), false)
      test.equal(method(), 1)
      test.equal(nested(), 2)
      test.equal(whole().kept, 3)
      ignore(whole().watched)
      test.pass()
      (-1.)
    elsif
      time() > deadline
    then
      test.fail(
        "Unused methods not collected after #{rounds()} rounds"
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
