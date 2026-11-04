# The help of a function prints a nullable bound to a nullable once.

# Picks `x`, else `default`.
def pick(x, (default:string?)) =
  x ?? default
end
