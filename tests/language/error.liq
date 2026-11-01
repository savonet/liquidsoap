%include "../test.liq"

def f() =
  # Works as expected with no errors
  ret =
    try
      4
    catch _ do
      5
    end

  test.equal(ret, 4)
  e = error.register("foo")

  # Works as expected with no errors
  ret =
    try
      4
    catch _ : [e] do
      5
    end

  test.equal(ret, 4)

  # Can report kind
  ret =
    try
      error.raise(e)
      "bla"
    catch err do
      err.kind
    end

  test.equal(ret, "foo")

  # Can report empty message
  ret =
    try
      error.raise(e)
      "bla"
    catch err do
      err.message
    end

  test.equal(ret, "")

  # Can report set message
  ret =
    try
      error.raise(e, "msg")
      "bla"
    catch err do
      err.message ?? "blo"
    end

  test.equal(ret, "msg")

  # Can report stack trace
  trace =
    try
      error.raise(e, "msg")
      []
    catch err do
      err.trace
    end

  pos =
    string.concat(
      separator=", ",
      list.map(fun (pos) -> pos.to_string(), trace)
    )

  test.equal(r/error.liq, line 60 char 4 - line 65 char 7/.test(pos), true)
  test.equal(r/thread.liq, line 21, char 11-14/.test(pos), true)
  e' = error.register("bla")
  test.equal(false, (e == e'))

  # Ignores errors when not in list
  ret =
    try
      try
        error.raise(e, "msg")
        "bla"
      catch _ : [e'] do
        "blo"
      end
    catch _ : [e] do
      "gni"
    end

  test.equal(ret, "gni")

  # Ignore errors when list is empty
  ret =
    try
      try
        error.raise(e, "msg")
        "bla"
      catch _ : [] do
        "blo"
      end
    catch _ : [e] do
      "gni"
    end

  test.equal(ret, "gni")

  # Catches error when in list
  ret =
    try
      try
        error.raise(e, "msg")
        "bla"
      catch _ : [e, e'] do
        "blo"
      end
    catch _ : [e] do
      "gni"
    end

  test.equal(ret, "blo")
  on_error = ref(error.register("dummy"))
  error.on_error(fun (e) -> on_error := error.methods(e))
  try
    error.raise(
      e,
      "On done callback"
    )
  catch _ : [e] do
    ()
  end

  if (on_error()).kind != e.kind then test.fail() end
  if
    (on_error()).message !=
      "On done callback"
  then
    test.fail()
  end

  def on_done() =
    test.pass()
  end

  # Works as expected with finally
  ret = ref(false)
  try
    ()
  finally
    ret := true
  end
  test.equal(ret(), true)

  ret = ref(false)
  try
    ()
  catch _ do
    ()
  finally
    ret := true
  end
  test.equal(ret(), true)

  ret = ref(false)
  try
    error.raise(error.not_found)
  catch _ do
    ()
  finally
    ret := true
  end
  test.equal(ret(), true)

  ret = ref(false)
  try
    error.raise(error.not_found)
  catch _ : [error.not_found] do
    ()
  finally
    ret := true
  end
  test.equal(ret(), true)

  ret = ref(false)
  try
    try
      error.raise(error.not_found)
    catch _ : [error.not_found] do
      error.raise(error.failure)
    finally
      ret := true
    end
  catch _ : [error.failure] do
    ()
  end
  test.equal(ret(), true)

  finally_runs = ref(0)
  propagated =
    try
      try
        error.raise(
          error.not_found,
          "no catch clause"
        )
      finally
        ref.incr(finally_runs)
      end
      "swallowed"
    catch err : [error.not_found] do
      err.message
    end
  test.equal(
    propagated,
    "no catch clause"
  )
  test.equal(finally_runs(), 1)

  finally_runs := 0
  propagated =
    try
      try
        error.raise(
          error.failure,
          "not in the catch list"
        )
      catch _ : [error.not_found] do
        ()
      finally
        ref.incr(finally_runs)
      end
      "swallowed"
    catch err : [error.failure] do
      err.message
    end
  test.equal(
    propagated,
    "not in the catch list"
  )
  test.equal(finally_runs(), 1)

  finally_runs := 0
  propagated =
    try
      try
        ()
      finally
        ref.incr(finally_runs)
        error.raise(
          error.failure,
          "raised by finally"
        )
      end
      "swallowed"
    catch err : [error.failure] do
      err.message
    end
  test.equal(
    propagated,
    "raised by finally"
  )
  test.equal(finally_runs(), 1)

  value =
    try
      42
    finally
      ()
    end
  test.equal(value, 42)

  # Catches error
  def on_error(~backtrace, e) =
    print(
      "caught error #{e} from thread and backtrace:\n#{backtrace}"
    )
    if error.methods(e).kind == "foo" then on_done() else test.fail() end
  end

  thread.on_error(e, on_error)
  thread.run(
    fun () ->
      error.raise(
        e,
        "Asynchronous error"
      )
  )
end

test.check(f)
