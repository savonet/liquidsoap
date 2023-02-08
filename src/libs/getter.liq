# Construct a function returning the value of a getter.
# @category Getter
def getter.function(x)
  {getter.get(x)}
end

# Determine if a getter is a constant.
# @category Getter
def getter.is_constant(x)
  getter.case(x, fun (_) -> true, fun (_) -> false)
end

# Convert an int getter to a float getter.
# @category Getter
def getter.float_of_int(x)
  getter.map(float_of_int, x)
end

# Convert a float getter to a int getter.
# @category Getter
def getter.int_of_float(x)
  getter.map(int_of_float, x)
end

# Execute a function when the value of the getter changes.
# @category Getter
def getter.on_change(f, x)
  x = {getter.get(x)}
  old = ref(x())
  fun () -> begin
    new = x()
    if old() != new then old := new; f(new) end
    new
  end
end

# Detect whether the value of the getter changes.
# @category Getter
def getter.changes(x)
  old = ref(getter.get(x))
  fun () -> begin
    new = getter.get(x)
    if old() != new then old := new; true
    else false end
  end
end

# Give the latest value among two getters.
# @category Getter
def getter.merge(x, y)
  v = ref(getter.get(x))
  x = getter.on_change(fun(x) -> v := x, x)
  y = getter.on_change(fun(y) -> v := y, y)
  fun () -> begin
    ignore(x())
    ignore(y())
    v()
  end
end
