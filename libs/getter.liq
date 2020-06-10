getter = ()

# Convert an int getter to a float getter.
# @category Math
def getter.float_of_int(x)
  x = to_int_getter(x)
  float_getter({float_of_int(x())})
end

# Convert a float getter to a int getter.
# @category Math
def getter.int_of_float(x)
  x = to_float_getter(x)
  int_getter({int_of_float(x())})
end
