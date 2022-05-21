a = ()

# My function
# @param ~x The parameter.
def a.b(~x)
  x
end

def f()
  ignore(a)
  test.pass()
end

test.check(f)
