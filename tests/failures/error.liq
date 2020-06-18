#!../../src/liquidsoap ../../libs/pervasives.liq

# Test uncaught errors.

def f() =
  error.raise("testing exceptions")
end

f()
