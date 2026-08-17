def base(~a=1, ~b=2) =
  a + b
end
def wrapper(%argsof(base)) =
  base(%argsof(base))
end
wrapper(a=3)
