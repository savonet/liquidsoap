# Compute the minimum of two values.
# @category Math
def min(a, b)
  if a <= b then a else b end
end

# Compute the maximum of two values.
# @category Math
def max(a, b)
  if a >= b then a else b end
end

# Float representation of infinity.
# @category Math
def infinity =
  infinity()
end

# Avoid unused variable warning
ignore(infinity)
