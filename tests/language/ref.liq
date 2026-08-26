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
  test.equal(n(), 5)
  s = ref("a")
  s := "b"
  test.equal(s(), "b")
  f = ref(id)
  f := incr
  b = ref(false)
  test.equal(b.exchange(true), false)
  test.equal(b.exchange(true), true)
  test.pass()
end

test.check(f)
