#!../../src/liquidsoap ../../libs/main.liq

# Test uncaught errors.

def f() =
  error.raise("testing exceptions")
end

f()
