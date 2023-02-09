# Create a reference from a pair of get / set functions.
# @category Programming
# @param get Function to retrieve the value of the reference.
# @param set Function to change the value of the reference.
def ref.make(get,set)
  (get.{set = set} : ref)
end

# Create a getter from a reference (sometimes useful to remove the `set`
# method).
# @category Programming
def ref.getter((r:ref)) =
  {r()}
end

# Map functions to a reference.
# @category Programming
# @param g Function to apply to the getter.
# @param s Function to apply to the setter.
def ref.map(g,s,(r:ref)) =
  ref.make({g(r())}, fun(x)->r.set(s(x)))
end

# Increment a reference to an integer.
# @category Programming
def ref.incr(r)
  r := r() + 1
end
