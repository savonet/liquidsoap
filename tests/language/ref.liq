#!../../liquidsoap ../test.liq

def id(x) =
  x
end

def incr(n) =
  n + 1
end

def f() =
  n = ref(0)
  n := 5
  test.equals(n(), 5)
  s = ref("a")
  s := "b"
  test.equals(s(), "b")
  f = ref(id)
  f := incr
  r = ref.make({0}, fun (x) -> ignore(x + 1))
  _ = ref.map(float_of_int, int_of_float, r)
  test.pass()
end

test.check(f)
