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
