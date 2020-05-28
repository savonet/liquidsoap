#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

def id(x) = x end
def incr(n) = n + 1 end

def f() =
  n = ref(0)
  n := 1
  s = ref("a")
  s := "b"
  f = ref(id)
  f := incr

  test.pass()
end

test.check(f)
