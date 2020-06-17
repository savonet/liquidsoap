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
  ret = try
    4
  catch _ do
    5
  end

  t(ret, 4)

  e = error.register("foo")
  t(e, error.register("foo"))

  ret = try
    error.raise(e);
    "bla"
  catch err do
    error.kind(err)
  end 
  t(ret, "foo")

  ret = try
    error.raise(e)
    "bla"
  catch err do
    error.message(err) | "blo"
  end
  t(ret, "blo")

  ret = try
    error.raise(e, "msg");
    "bla"
  catch err do
    error.message(err) | "blo"
  end
  t(ret, "msg")

  e' = error.register("bla")
  t(false, (e == e'))

  ret =
    try
      try
        error.raise(e, "msg");
        "bla"
      catch err: [e'] do
        "blo"
      end
    catch err: [e] do
      "gni"
    end
  t(ret, "gni")

  ret =
    try
      try
        error.raise(e, "msg");
        "bla"
      catch err: [e, e'] do
        "blo"
      end
    catch err: [e] do
      "gni"
    end
  t(ret, "blo")
  
  if !success then test.pass() else test.fail() end
end

test.check(f)
