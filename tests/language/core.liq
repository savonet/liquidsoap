#!../../src/liquidsoap --debug-lang

# Simple tests without the standard library.

def pass()
  print("Test passed!")
  shutdown()
end

def fail()
  print("Test failed!")
  exit(1)
end

m = 2
n = 2 + 2
suc = fun (x) -> x + 1
id = fun(x) -> x

def rec f(x)
  f(x)
end