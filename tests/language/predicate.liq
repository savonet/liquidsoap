#!../src/liquidsoap ../libs/pervasives.liq

%include "test.liq"

success = ref(true)

def predicate.to_list(n, p)
  list.init(n, fun(_) -> p())
end

def predicate.nth(n)
  i = ref(-1)
  fun () -> begin
    ref.incr(i)
    !i == n
  end
end

def predicate.from(n)
  i = ref(-1)
  fun () -> begin
    ref.incr(i)
    !i >= n
  end
end

def test(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

def testl(p, l)
  test(predicate.to_list(4, p), l)
end

def f() =
  testl(predicate.nth(2), [false,false,true,false])
  testl(predicate.from(2), [false,false,true,true])
  testl(predicate.activates(predicate.from(2)), [false,false,true,false])
  testl(predicate.changes(predicate.nth(2)), [false,true,true,false])
  testl(predicate.once(predicate.from(2)), [false,false,true,false])

  if !success then test.pass() else test.fail() end
end

test.check(f)
