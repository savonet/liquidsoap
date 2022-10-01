# Create a getter from a reference.
# @category Programming
def ref.getter(r) =
  {!r}.{set = fun(v) -> r := v}
end

# Increment a reference to an integer.
# @category Programming
def ref.incr(r)
  r := !r + 1
end
