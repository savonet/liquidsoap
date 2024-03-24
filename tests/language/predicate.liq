def predicate.to_list(n, p) =
  list.init(n, fun (_) -> p())
end

def predicate.nth(n) =
  i = ref(-1)
  fun () ->
    begin
      ref.incr(i)
      i() == n
    end
end

def predicate.from(n) =
  i = ref(-1)
  fun () ->
    begin
      ref.incr(i)
      i() >= n
    end
end

def tl(p, l) =
  test.equal(predicate.to_list(4, p), l)
end

def f() =
  tl(predicate.nth(2), [false, false, true, false])
  tl(predicate.from(2), [false, false, true, true])
  tl(predicate.activates(predicate.from(2)), [false, false, true, false])
  tl(predicate.changes(predicate.nth(2)), [false, true, true, false])
  tl(predicate.once(predicate.from(2)), [false, false, true, false])
  p = predicate.signal()
  test.equal(p(), false)
  p.signal()
  test.equal(p(), true)
  test.equal(p(), false)

  test.pass()
end

test.check(f)
