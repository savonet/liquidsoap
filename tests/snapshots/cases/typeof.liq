x = {foo = {bar = 1}}
def f((v : %typeof(x.foo.bar))) =
  v
end
(f(2) : %typeof(x).foo.bar)
