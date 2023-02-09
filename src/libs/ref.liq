# Create a reference from a pair of get / set functions.
# @category Programming
# @param get Function to retrieve the value of the reference.
# @param set Function to change the value of the reference.
def ref.make(get,set)
  get.{set = set}
end

# Create a getter from a reference (sometimes useful to remove the `set`
# method).
# @category Programming
def ref.getter(r) =
  {r()}
end

# Increment a reference to an integer.
# @category Programming
def ref.incr(r)
  r := r() + 1
end
