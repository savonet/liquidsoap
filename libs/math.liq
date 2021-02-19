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

# Convert linear scale into decibels.
# @category Math
def dB_of_lin(x)
  20. * log10(x)
end

# Convert decibels into linear scale.
# @category Math
def lin_of_dB(x)
  pow(10., x / 20.)
end
