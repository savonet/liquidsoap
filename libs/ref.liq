# Create a reference, i.e. a value that can be modified. This is an alias to `ref`.
# @category Liquidsoap 
def ref.make(x) =
  ref x
end

# Create a getter from a reference.
# @category Liquidsoap
def ref.getter(r) =
  fun () -> !r
end

# Increment a reference to an integer.
# @category Liquidsoap
def ref.incr(r)
  r := !r + 1
end
