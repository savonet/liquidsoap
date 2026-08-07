# An explicit `begin ... end` must survive as a block node: a `def` body is an
# implicit block and flattens into `body`, but a `fun` body is a single
# expression, so a block there was written by hand and the formatter would
# delete it if it were flattened too.
f =
  fun () ->
    begin
      a = 1
      a
    end

def g() =
  b = 2
  b
end

# A ternary's branches are expression nodes, not statement arrays.
l = [1, ...(true ? [2] : []), 3]
h = true ? "yes" : "no"

(f(), g(), l, h)
