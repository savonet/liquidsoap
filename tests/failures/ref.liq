#!../../liquidsoap

# Ensure that value restriction prevents the usual problems.

def id(x) = x end

def incr(n) = n+1 end

r = ref(id)
r := incr
f = !r
_ = f("a")
