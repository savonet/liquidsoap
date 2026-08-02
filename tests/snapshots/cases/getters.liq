def f(x={int}) =
  x()
end
f(fun () -> 3)
