#!../../liquidsoap

%include "../test.liq"

dropped_fired = ref(false)
derived_fired = ref(false)
operator_fired = ref(false)
string_fired = ref(false)
string_received = ref("")
function_received = ref(0)
self_received = ref("")

def drop_value() =
  f = fun () -> ()
  runtime.gc.finalise.last({dropped_fired := true}, f)
end

def drop_string() =
  scratch = "scratch" ^ string(random.int())
  runtime.gc.finalise.last({string_fired := true}, scratch)
end

# Only a copy with a method added survives: it holds the same function.
def keep_derived() =
  x = fun () -> ()
  runtime.gc.finalise.last({derived_fired := true}, x)
  let x.foo = 1
  x
end

# The operator holds a copy of the predicate rebuilt with its own types.
def keep_in_operator() =
  p = fun () -> true
  runtime.gc.finalise.last({operator_fired := true}, p)
  switch([(p, blank())])
end

# finalise hands the registered value back.
def drop_for_finalise() =
  s = "received" ^ string(random.int(min=0, max=1))
  runtime.gc.finalise(fun (s) -> string_received := s, s)

  f = fun () -> 42
  runtime.gc.finalise(fun (f) -> function_received := f(), f)

  # A method referring to the value's own data must not keep it alive.
  data = "self" ^ string(random.int(min=0, max=1))
  def get() =
    data
  end
  let data.get = get
  runtime.gc.finalise(fun (v) -> self_received := v.get(), data)
end

def f() =
  # Ignored with a warning: these hold no data of their own.
  runtime.gc.finalise.last({()}, 3)
  runtime.gc.finalise.last({()}, null)
  runtime.gc.finalise(fun (_) -> (), [])

  drop_value()
  drop_string()
  drop_for_finalise()
  derived = keep_derived()
  held = keep_in_operator()
  deadline = time() + 20.
  rounds = ref(0)

  def check() =
    runtime.gc.full_major()
    ref.incr(rounds)
    if
      dropped_fired()
      and string_fired()
      and self_received() != ""
      and string_received() != ""
      and function_received() != 0
      and rounds() >= 3
    then
      test.equal(derived_fired(), false)
      test.equal(operator_fired(), false)
      test.equal(derived.foo, 1)
      test.equal(string.sub(string_received(), start=0, length=8), "received")
      test.equal(function_received(), 42)
      test.equal(string.sub(self_received(), start=0, length=4), "self")
      ignore(held)
      test.pass()
      (-1.)
    elsif
      time() > deadline
    then
      test.fail(
        "Dropped values not collected after #{rounds()} rounds"
      )
      (-1.)
    else
      0.5
    end
  end

  thread.run.recurrent(delay=0., check)
end

test.check(f)
