# Passing int to a function expecting float
def double(x) =
  2. * x
end
ten = double(5)

# Math builtins work with int arguments
x = sqrt(4)
y = sin(0)

# Named float arguments accept int
def with_float(~x, ~y=2.) =
  x + y
end
result = with_float(x=3, y=4)
