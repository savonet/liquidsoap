# `{e}` is shorthand for `fun () -> e`, which is what makes a getter.
def f(x) =
  getter.get(x)
end

(f(1), f({2}))
