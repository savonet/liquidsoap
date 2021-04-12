#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end
  
def f() =
  # Works as expected with no errors
  ret = try
    4
  catch _ do
    5
  end

  t(ret, 4)

  e = error.register("foo")
  t(e, error.register("foo"))

  # Works as expected with no errors
  ret = try
    4
  catch _ in [e] do
    5
  end

  t(ret, 4)

  # Can report kind
  ret = try
    error.raise(e);
    "bla"
  catch err do
    error.kind(err)
  end 
  t(ret, "foo")

  # Can report empty message
  ret = try
    error.raise(e)
    "bla"
  catch err do
    error.message(err) ?? "blo"
  end
  t(ret, "blo")

  # Can report set message
  ret = try
    error.raise(e, "msg");
    "bla"
  catch err do
    error.message(err) ?? "blo"
  end
  t(ret, "msg")

  e' = error.register("bla")
  t(false, (e == e'))

  # Ignores errors when not in list
  ret =
    try
      try
        error.raise(e, "msg");
        "bla"
      catch err in [e'] do
        "blo"
      end
    catch err in [e] do
      "gni"
    end
  t(ret, "gni")

  # Ignore errors when list is empty
  ret =
    try
      try
        error.raise(e, "msg");
        "bla"
      catch err in [] do
        "blo"
      end
    catch err in [e] do
      "gni"
    end
  t(ret, "gni")

  # Catches error when in list
  ret =
    try
      try
        error.raise(e, "msg");
        "bla"
      catch err in [e, e'] do
        "blo"
      end
    catch err in [e] do
      "gni"
    end
  t(ret, "blo")

  def on_done() =
    if !success then
      test.pass()
    else
      test.fail()
    end 
  end

  # Catches error in asynchronous tasks
  def on_error(~backtrace, ~queue_name, e) =
    print("caught error #{e} from queue #{queue_name} and backtrace:\n#{backtrace}")
    if error.kind(e) == "foo" then
      on_done()
    else
      test.fail()
    end
  end

  thread.on_error(e, on_error)
  thread.run(fun () -> error.raise(e, "Asynchronous error"))
end

test.check(f)
