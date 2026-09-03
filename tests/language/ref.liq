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

  # A reference over foreign state: every operation is an atomic section.
  cell = ref(0)
  m = ref.make({cell()}, fun (x) -> cell.set(x))
  m := 7
  test.equal(m(), 7)
  test.equal(m.exchange(8), 7)
  test.equal(cell(), 8)
  doubled = ref.map(fun (x) -> 2 * x, fun (x) -> x / 2, m)
  test.equal(doubled(), 16)

  test.pass()
end

test.check(f)
