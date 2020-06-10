# "Delayed" version of `maybe.case`, where the default value is only evaluated
# if necessary.
# @category Liquidsoap
# @param x Optional value.
# @param d Value to evaluate in case of nothing.
# @param f Value to produce in case of something.
def maybe.dcase(x, d, f)
  f = maybe.case(x, d, fun(x) -> {f(x)})
  f()
end

# Get the value of an optional value and raise an error if the value is nothing.
# @category Liquidsoap
# @param x Value.
# @param e Error to raise.
def maybe.raise(x, e) =
  maybe.dcase(x, {error.raise(e)}, fun(x) -> x)
end
