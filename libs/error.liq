let error.not_found = error.register("not_found")

let error.assertion = error.register("assertion")

# Ensure that a condition is satisfied (raise `error.assertion` exception
# otherwise).
# @category Liquidsoap
# @param c Condition which should be satisfied.
def assert(c)
  if not c then error.raise(error.assertion, "Assertion failed.") end
end

let error.failure = error.register("failure")

# Major failure.
# @category Liquidsoap
# @param msg Explanation about the failure.
def failwith(msg)
  error.raise(error.failure, msg)
end
