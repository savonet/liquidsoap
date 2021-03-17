let error.not_found = error.register("not_found")

let error.assertion = error.register("assertion")

# Ensure that a condition is satisfied (raise `error.assertion` exception
# otherwise).
# @category Liquidsoap
# @param
def assert(c)
  if not c then error.raise(error.assertion) end
end
