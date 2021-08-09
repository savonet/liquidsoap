#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Test uncaught errors.

def f() =
  error.raise("testing exceptions")
end

f()
