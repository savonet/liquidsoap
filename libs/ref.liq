# Create a getter from a reference.
# @category Liquidsoap
def ref.getter(r) =
  {!r}.{set = fun(v) -> r := v}
end

# Increment a reference to an integer.
# @category Liquidsoap
def ref.incr(r)
  r := !r + 1
end
