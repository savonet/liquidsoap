x = 1
def f(a, ~b=x) =
  x = a + b
  x
end
def rec count(n) =
  if n == 0 then 0 else count(n - 1) end
end
let (p, q) = (f(1), count(2))
s = sine()
s.label = "radio"
print((x, p, q, s))

#? definition 3:6
#? definition 3:10
#? definition 2:12
#? definition 4:2
#? definition 7:24
#? definition 7:30
#? definition 12:7
#? definition 12:10
#? definition 12:16
#? definition 10:4
#? definition 1:0
