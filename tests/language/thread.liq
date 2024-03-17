def f() =
  count = ref(0)

  def cb() =
    count := count() + 1
  end

  thread.when({true}, cb)
  thread.pause(1.)
  test.equals(count(), 1)

  # Testing a step with initial true:
  # True for the first 2s then false for 2s, then true
  t = time()

  def p() =
    cur_t = time()
    (t <= cur_t and cur_t < t + 2.) or t + 4. <= cur_t
  end

  count = ref(0)

  def cb() =
    count := count() + 1
  end

  thread.when(init=false, p, cb)
  thread.pause(5.)
  test.equals(count(), 1)

  # Testing a step with initial false:
  # false for the first 2s then true
  t = time()

  def p() =
    t + 2. <= time()
  end

  count = ref(0)

  def cb() =
    count := count() + 1
  end

  thread.when(p, cb)
  thread.pause(3.)
  test.equals(count(), 1)

  # Testing when triggering on each change
  # True for the first 2s then false
  t = time()

  def p() =
    cur_t = time()
    t <= cur_t and cur_t < t + 2.
  end

  count = ref(0)

  def cb() =
    count := count() + 1
  end

  thread.when(changed=false, every=0.5, p, cb)
  thread.pause(5.)
  test.equals(count(), 4)

  # Make sure that after_eval is executed in the thread.
  err = error.register("after_eval")
  error_seen = ref(false)
  def fn() =
    try
      liquidsoap.after_eval(
        {
          error.raise(
            err,
            "After eval"
          )
        }
      )
    catch _ : [err] do
      ()
    end
  end
  def on_error(_) =
    error_seen := true
  end
  thread.run(on_error=on_error, fn)
  while not error_seen() do
    thread.delay(0.1)
  end

  test.pass()
end

test.check(f)
