def f(~x=null, ~y=2, z) =
  (x ?? 0) + y + z
end

f(y=5, 1)
