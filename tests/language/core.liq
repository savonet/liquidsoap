#!../../src/liquidsoap

# Simple tests without the standard library.

def pass()
  print("Test passed!")
  shutdown()
end

def fail()
  print("Test failed!")
  exit(1)
end
