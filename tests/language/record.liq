#!../../liquidsoap ../test.liq

def f() =
  # Basic checks
  n = 2
  r = n.{ a = 8 , b = 12 , f = fun(x) -> 2 * x }
  test.equals(1+r, 3)
  test.equals(r.b, 12)
  test.equals(r.a, 8)
  test.equals(r.f(5), 10)
  r = 2.{a = 5}
  ignore(r)

  # Test overriding with subfields
  r = ()
  let r.f = ()
  let r.f.variant = 7
  let r.f = r.f.variant
  ignore(r)

  # Test replacing subfields
  r = ()
  let r.a = ()
  let r.a.b = 5
  let replaces r.a = 2
  ignore(r.a.b)
  def replaces r.a (x) = 2*x end
  ignore(r.a.b)

  # A resettable counter
  n = ref(0)
  def counter()
    n := !n + 1
    !n
  end
  def counter.reset()
    n := 0
  end

  ignore(counter())
  ignore(counter())
  test.equals(counter(), 3)
  counter.reset()
  test.equals(counter(), 1)

  # Float / field disambiguation
  r = 3.{s = "a"}
  _ = r.s

  # Open
  r = ()
  def r.f(n) = 2*n end
  open r
  test.equals(f(3), 6)

  # Test subtyping in lists
  a = "a"
  b = "b"
  let a.x = 5
  let b.x = 3
  let b.y = 1.
  l = [a,b]
  ignore(list.hd(l).x)
  l = [b,a]
  ignore(list.hd(l).x)
  def f(c) = [a,c] end
  ignore(f)

  # Subsequent increase of the type
  _ = if true then {a = 4, b = 5} else {a = 4} end
  _ = if true then {a = 4} else {a = 4, b = 5} end

  # Fields with incompatible types
  _ = [{a = 4, b = 5, c = 6}, {a = "a", b = 2, d = "d"}]

  # Equality
  test.equals({a = 5} == {a = 5}, true)
  test.equals({a = 5} == {a = 6}, false)
  test.equals({a = 5} == {a = 6, b = 4}, false)
  test.equals([{a = 5}] == [{a = 5}], true)
  test.equals(({a = 5}) == ({a = 5}), true)
  # The following is weird but expected
  test.equals({a = 5} == {a = 5, b = 6}, true)
  test.equals("bla".{x = 2} == "bla", true)
  test.equals("bla" == "bla".{x = 2}, true)

  # Test optional fields
  def f(x) =
    (x?.foo ?? 2) + 1
  end
  test.equals(f(()), 3)
  test.equals(f({}), 3)
  test.equals(f({foo=1}), 2)
  test.equals(f({foo=null()}), 3)

  def f(x) =
   ret = x?.foo(123)
   ret ?? 1
  end
  test.equals(f(()), 1)
  test.equals(f({}), 1)
  test.equals(f({foo=(fun (x) -> x)}), 123)
  test.equals(f({foo=null()}), 1)

  def f(x) =
    [x, {foo = 123}]
  end
  ignore(list.hd(f(()))?.foo)
  ignore((f({foo = 345}):[{foo?:int}]))

  x = { }
  ignore(x.foo ?? 123)

  def f(x) =
    ignore((x.add ?? 1))
    if false then x else () end
    x
  end
  x = f(())
  (x:{add?:int})

  x = (():{foo?:int?})
  ignore(f(()))

  def f(x) =
    x.foo.gni.bla(1,2,3).blo
  end

  test.equals(f({ foo = { gni = { bla = (fun (_,_,_,~foo="gni") -> { blo = 123 })}}}), 123)

  def f(x) =
    x.foo?.gni.bla(1,2,3)
  end
  test.equals(f({ foo = null() }), null())

  # We want to make sure that:
  def f(x) =
    x?.foo
  end
  # is properly typed as: ('B.{foo? : 'A}) -> 'A?
  test.equals(f({}), null())

  x = ({foo = null()}:{foo: int?})
  test.equals(f(x), null())

  x = ({foo = null()}:{foo?:int?})
  test.equals(f(x), null())

  test.equals(f(true), null())
  test.equals(f({foo = 1}), null(1))

  # We want to make sure that:
  def f(x) =
    x?.foo(1)?.gni
  end
  # is properly typed as: ('A.{foo? : (int) -> 'B.{gni? : 'C}}) -> 'C?
  # i.e. that { foo = (fun (_) -> { } ) } is a valid argument
  test.equals(f({ foo = (fun (_) -> { } ) }), null())
  test.equals(f(112.{ foo = (fun (_) -> false ) }), null())
  test.equals(f({ foo = (fun (_) -> { gni = 2 } ) }), null(2))
  test.equals(f(345.{ foo = (fun (_) -> "aabb".{ gni = 2 } ) }), null(2))

  # We want to make sure that:
  def f(x) =
    x?.foo(1).gni ?? 1
  end
  # is properly typed as: ('A.{foo? : (int) -> 'B.{gni? : int}}) -> int
  # i.e. that { foo = (fun (_) -> { } ) } is a valid argument
  test.equals(f({}), 1)
  test.equals(f(2), 1)
  test.equals(f({ foo = (fun (_) -> { } ) }), 1)
  test.equals(f("aabb".{ foo = (fun (_) -> 123 ) }), 1)
  test.equals(f({ foo = (fun (_) -> { gni = 2 } ) }), 2)
  test.equals(f(456.{ foo = (fun (_) -> true.{ gni = 2 } ) }), 2)

  # Spread patterns:
  x = { foo = 123, gni = "aabb" }
  let { foo, ...y } = x
  test.equals(foo, 123)
  test.equals(y.gni, "aabb")
  test.equals(y, ())
  test.equals(y?.foo, null())

  x = 1.{ foo = 123, gni = "aabb" }
  let { foo, ...y } = x
  test.equals(foo, 123)
  test.equals(y.gni, "aabb")
  test.equals(y, 1)
  test.equals(y?.foo, null())

  x = 1.{ foo = 123, gni = "aabb" }
  let y.{ foo } = x
  test.equals(foo, 123)
  test.equals(y.gni, "aabb")
  test.equals(y, 1)
  test.equals(y?.foo, null())

  x = {foo = 3.14}.{ foo = 123, gni = "aabb" }
  let { foo, ...y } = x
  test.equals(foo, 123)
  test.equals(y.gni, "aabb")
  test.equals(y, ())
  test.equals(y?.foo, null())

  x = { foo = 123, gni = "aabb" }
  y = { bla = 3.14, ...x }
  test.equals(y, { foo = 123, gni = "aabb", bla = 3.14 })

  x = 1.{ foo = 123, gni = "aabb" }
  y = { bla = 3.14, ...x }
  test.equals(y, 1.{ foo = 123, gni = "aabb", bla = 3.14 })

  # Make sure that a function that takes a record of the
  # type { foo?: int} can take a record of the type: {foo?: never}
  def f(x) =
    (x?.foo ?? 1) + 2
  end
  x = {foo = 123}
  let {foo = _, ...y} = x
  test.equals(f(y), 3)

  # Make sure that we infer optional methods correctly
  def f(x) =
    ignore(x?.foo)
  end

  def g(x) =
    f(x.{gni=123})
  end
  g({blo="bla"})

  test.pass()
end

test.check(f)
