# A "for" loop: call multiple times a function with increasing integers as
# arguments, between specified bounds. For instance, `for(5, 8, f)` is
# equivalent to `f(5); f(6); f(7); f(8)`.
# @param first First number
# @param last Last number
# @param f Function to be called
# @category Liquidsoap
def rec for(first, last, f)
  if first <= last then
    f(first)
    for(first+1, last, f)
  end
end

# A "while" loop: call multiple times a function as long as a condition is
# satisfied.
# @param cond Boolean condition
# @param f Function to be called
# @cateogry Liquidsoap
def rec while(cond, f)
  c = to_bool_getter(cond)
  if c() then
    f()
    while(cond, f)
  end
end
