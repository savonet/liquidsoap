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

# A special floating-point value denoting the result of an undefined operation
# such as 0.0 /. 0.0. Stands for 'not a number'. Any floating-point operation
# with nan as argument returns nan as result. As for floating-point comparisons, 
# `==`, `<`, `<=`, `>` and `>=` return `false` and `!=` returns `true` if one or
# both of their arguments is `nan`.
# @category Math
def nan
  sqrt(-1.)
end
