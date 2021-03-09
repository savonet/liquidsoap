# Construct a function returning the value of a getter.
# @category Liquidsoap
def getter.function(x)
  {getter.get(x)}
end

def getter.is_constant(x)
  getter.elim(x, fun (_) -> true, fun (_) -> false)
end

# Apply a function to a getter.
# @category Liquidsoap
def getter.map(f, x)
  getter(f(getter.get(x)))

  # TODO: both optimizations below are not typable

  # getter.elim(x, fun(x) -> getter(f(x)), fun (x) -> getter({f(x())}))

  # if getter.is_constant(x) then
    # getter(f(getter.get(x)))
  # else
    # getter({f(getter.get(x))})
  # end
end

# Convert an int getter to a float getter.
# @category Math
def getter.float_of_int(x)
  getter.map(float_of_int, x)
end

# Convert a float getter to a int getter.
# @category Math
def getter.int_of_float(x)
  getter.map(int_of_float, x)
end

# Execute a function when the value of the getter changes.
# @category Math
def getter.on_change(f, x)
  x = {getter.get(x)}
  old = ref(x())
  fun () -> begin
    new = x()
    if !old != new then old := new; f(new) end
    new
  end
end

# Detect whether the value of the getter changes.
# @category Math
def getter.changes(x)
  old = ref(getter.get(x))
  fun () -> begin
    new = getter.get(x)
    if !old != new then old := new; true
    else false end
  end
end
