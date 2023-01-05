# Determine whether a nullable value is not null.
# @category Programming
def null.defined(x) =
  null.case(x, {false}, fun (_) -> true)
end

# Get the value of a nullable. Raises `error.not_found` if the value is `null`
# and no default value was specified.
# @category Programming
# @param ~default Returned value when the value is `null`.
def null.get(~default=null(), x)
  null.case(x,
    {default ?? error.raise(error.not_found, "no default value for null.get")},
    fun (x) -> x)
end

# Convert a nullable value to a list containing zero or one element depending on
# whether the value is null or not.
# @category Programming
def null.to_list(x)
  null.case(x, {[]}, fun (x) -> [x])
end

# Apply a function on a nullable value if it is not null, and return null
# otherwise.
# @category Programming
def null.map(f,x)
  null.case(x, {null()}, fun(x) -> f(x))
end

# Find the first element of a list for which the image of the function is not
# `null`. Raises `error.not_found` if not element is found and no default value
# was specified.
# @category Programming
# @param ~default Returned value when no element is found.
# @param f Function.
# @param l List.
def null.find(~default=null(), f, l)
  def rec aux(l)
    f = list.case(l,
      {default ?? error.raise(error.not_found, "no default value for list.find_defined")},
      fun(x, l) -> {begin y = f(x); if null.defined(y) then y else aux(l) end end})
    f()
  end
  aux(l)
end
