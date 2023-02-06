def f() =
  # Works as expected with no errors
  ret = try
    4
  catch _ do
    5
  end

  test.equals(ret, 4)

  e = error.register("foo")

  # Works as expected with no errors
  ret = try
    4
  catch _ : [e] do
    5
  end

  test.equals(ret, 4)

  # Can report kind
  ret = try
    error.raise(e);
    "bla"
  catch err do
    err.kind
  end
  test.equals(ret, "foo")

  # Can report empty message
  ret = try
    error.raise(e)
    "bla"
  catch err do
    err.message
  end
  test.equals(ret, "")

  # Can report set message
  ret = try
    error.raise(e, "msg");
    "bla"
  catch err do
    err.message ?? "blo"
  end
  test.equals(ret, "msg")

  # Can report stack trace
  trace = try
    error.raise(e, "msg");
    []
  catch err do
    err.trace
  end
  pos = string.concat(separator=", ", list.map(fun (pos) -> pos.to_string(), trace))
  test.equals(pos, "At error.liq, line 50 char 10 - line 55 char 5")

  e' = error.register("bla")
  test.equals(false, (e == e'))

  # Ignores errors when not in list
  ret =
    try
      try
        error.raise(e, "msg");
        "bla"
      catch _ : [e'] do
        "blo"
      end
    catch _ : [e] do
      "gni"
    end
  test.equals(ret, "gni")

  # Ignore errors when list is empty
  ret =
    try
      try
        error.raise(e, "msg");
        "bla"
      catch _ : [] do
        "blo"
      end
    catch _ : [e] do
      "gni"
    end
  test.equals(ret, "gni")

  # Catches error when in list
  ret =
    try
      try
        error.raise(e, "msg");
        "bla"
      catch _ : [e, e'] do
        "blo"
      end
    catch _ : [e] do
      "gni"
    end
  test.equals(ret, "blo")

  on_error = ref(error.register("dummy"))
  error.on_error(fun (e) -> on_error := e)

  try
    error.raise(e, "On done callback")
  catch _ : [e] do
    ()
  end

  if (on_error()).kind != e.kind then
    test.fail()
  end

  if (on_error()).message != "On done callback" then
    test.fail()
  end

  def on_done() =
    test.pass()
  end

  # Catches error
  def on_error(~backtrace, e) =
    print("caught error #{e} from thread and backtrace:\n#{backtrace}")
    if e.kind == "foo" then
      on_done()
    else
      test.fail()
    end
  end

  thread.on_error(e, on_error)
  thread.run(fun () -> error.raise(e, "Asynchronous error"))
end

test.check(f)
