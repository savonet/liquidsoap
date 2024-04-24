let error.assertion = error.register("assertion")
let error.clock = error.register("clock")
let error.eval = error.register("eval")
let error.file = error.register("file")
let error.file.cross_device = error.register("file.cross_device")
let error.http = error.register("http")
let error.invalid = error.register("invalid")
let error.json = error.register("json")
let error.not_found = error.register("not_found")
let error.output = error.register("output")
let error.socket = error.register("socket")
let error.string = error.register("string")

# Ensure that a condition is satisfied (raise `error.assertion` exception
# otherwise).
# @category Programming
# @param c Condition which should be satisfied.
def assert(c) =
  if
    not c
  then
    error.raise(
      error.assertion,
      "Assertion failed."
    )
  end
end

let error.failure = error.register("failure")

# Major failure.
# @category Programming
# @param msg Explanation about the failure.
def failwith(msg) =
  error.raise(error.failure, msg)
end

# Ensure that a given computation is always executed
# @category Programming
def ensure(~finally, fn) =
  finally_called = ref(false)
  try
    ret = fn()
    finally_called := true
    finally()
    ret
  catch err do
    if not finally_called() then finally() end
    error.raise(err)
  end
end
