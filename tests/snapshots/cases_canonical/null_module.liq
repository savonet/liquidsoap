# The preprocessor reads `null.m` and `null(x)` through a module; formatting
# must print `null` back.
def null.twice(x) =
  null.get(x) * 2
end
y = null(1)
z = null.twice(y)
