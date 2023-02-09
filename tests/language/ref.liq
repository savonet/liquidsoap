#!../../liquidsoap ../test.liq

success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

def id(x) = x end
def incr(n) = n + 1 end

def f() =
  n = ref(0)
  n := 5
  t(n(), 5)
  s = ref("a")
  s := "b"
  t(s(), "b")
  f = ref(id)
  f := incr

  r = ref.make({0},fun(x)->ignore(x+1))
  _ = ref.map(float_of_int, int_of_float, r)

  if success() then test.pass() else test.fail() end
end

test.check(f)
