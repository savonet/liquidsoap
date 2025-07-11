#!../../liquidsoap ../test.liq
def f() =
  # Basic checks
  n = 2
  r = n.{a=8, b=12, f=fun (x) -> 2 * x}
  test.equal(1 + r, 3)
  test.equal(r.b, 12)
  test.equal(r.a, 8)
  test.equal(r.f(5), 10)
  r = 2.{a=5}
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

  def replaces r.a(x) =
    2 * x
  end

  ignore(r.a.b)

  # A resettable counter
  n = ref(0)

  def counter() =
    n := n() + 1
    n()
  end

  def counter.reset() =
    n := 0
  end

  ignore(counter())
  ignore(counter())
  test.equal(counter(), 3)
  counter.reset()
  test.equal(counter(), 1)

  # Float / field disambiguation
  r = 3.{s="a"}
  _ = r.s

  # Open
  r = ()

  def r.f(n) =
    2 * n
  end

  open r
  test.equal(f(3), 6)

  # Test subtyping in lists
  a = "a"
  b = "b"
  let a.x = 5
  let b.x = 3
  let b.y = 1.
  l = [a, b]
  ignore(list.hd(l).x)
  l = [b, a]
  ignore(list.hd(l).x)

  def f(c) =
    [a, c]
  end

  ignore(f)

  # Subsequent increase of the type
  _ = if true then {a=4, b=5} else {a=4} end
  _ = if true then {a=4} else {a=4, b=5} end

  # Fields with incompatible types
  _ = [{a=4, b=5, c=6}, {a="a", b=2, d="d"}]

  # Equality
  test.equal({a=5} == {a=5}, true)
  test.equal({a=5} == {a=6}, false)
  test.equal({a=5} == {a=6, b=4}, false)
  test.equal([{a=5}] == [{a=5}], true)
  test.equal(({a=5}) == ({a=5}), true)

  # The following is weird but expected
  test.equal({a=5} == {a=5, b=6}, true)
  test.equal("bla".{x=2} == "bla", true)
  test.equal("bla" == "bla".{x=2}, true)

  # Test optional fields
  def f(x) =
    (x?.foo ?? 2) + 1
  end

  test.equal(f(()), 3)
  test.equal(f({}), 3)
  test.equal(f({foo=1}), 2)
  test.equal(f({foo=null}), 3)

  def f(x) =
    ret = x?.foo(123)
    ret ?? 1
  end

  test.equal(f(()), 1)
  test.equal(f({}), 1)
  test.equal(f({foo=(fun (x) -> x)}), 123)
  test.equal(f({foo=null}), 1)

  def f(x) =
    [x, {foo=123}]
  end

  ignore(list.hd(f(()))?.foo)
  ignore((f({foo=345}) : [{foo?: int}]))
  x = {}
  ignore(x.foo ?? 123)

  def f(x) =
    ignore((x.add ?? 1))
    if false then x else () end
    x
  end

  x = f(())
  (x : {add?: int})
  x = (() : {foo?: int?})
  ignore(f(()))

  def f(x) =
    x.foo.gni.bla(1, 2, 3).blo
  end

  test.equal(
    f(
      {
        foo=
          {
            gni=
              {
                bla=
                  (
                    fun (_, _, _, ~foo="gni") ->
                      begin
                        ignore(foo)
                        {blo=123}
                      end
                  )
              }
          }
      }
    ),
    123
  )

  def f(x) =
    x.foo?.gni.bla(1, 2, 3)
  end

  test.equal(f({foo=null}), null)

  # We want to make sure that:
  def f(x) =
    x?.foo
  end

  # is properly typed as: ('B.{foo? : 'A}) -> 'A?
  test.equal(f({}), null)
  x = ({foo=null} : {foo: int?})
  test.equal(f(x), null)
  x = ({foo=null} : {foo?: int?})
  test.equal(f(x), null)
  test.equal(f(true), null)
  test.equal(f({foo=1}), null(1))

  # We want to make sure that:
  def f(x) =
    x?.foo(1)?.gni
  end

  # is properly typed as: ('A.{foo? : (int) -> 'B.{gni? : 'C}}) -> 'C?
  # i.e. that { foo = (fun (_) -> { } ) } is a valid argument
  test.equal(f({foo=(fun (_) -> {})}), null)
  test.equal(f(112.{foo=(fun (_) -> false)}), null)
  test.equal(f({foo=(fun (_) -> {gni=2})}), null(2))
  test.equal(f(345.{foo=(fun (_) -> "aabb".{gni=2})}), null(2))

  # We want to make sure that:
  def f(x) =
    x?.foo(1).gni ?? 1
  end

  # is properly typed as: ('A.{foo? : (int) -> 'B.{gni? : int}}) -> int
  # i.e. that { foo = (fun (_) -> { } ) } is a valid argument
  test.equal(f({}), 1)
  test.equal(f(2), 1)
  test.equal(f({foo=(fun (_) -> {})}), 1)
  test.equal(f("aabb".{foo=(fun (_) -> 123)}), 1)
  test.equal(f({foo=(fun (_) -> {gni=2})}), 2)
  test.equal(f(456.{foo=(fun (_) -> true.{gni=2})}), 2)

  # Spread patterns:
  x = {foo=123, gni="aabb"}
  let {foo, ...y} = x
  test.equal(foo, 123)
  test.equal(y.gni, "aabb")
  test.equal(y, {gni="aabb"})
  test.equal(y?.foo, null)
  x = 1.{foo=123, gni="aabb"}
  let {foo, ...y} = x
  test.equal(foo, 123)
  test.equal(y.gni, "aabb")
  test.equal(y, 1)
  test.equal(y?.foo, null)
  x = 1.{foo=123, gni="aabb"}
  let {foo, ...y} = x
  test.equal(foo, 123)
  test.equal(y.gni, "aabb")
  test.equal(y, 1)
  test.equal(y?.foo, null)
  x = {foo=3.14}.{foo=123, gni="aabb"}
  let {foo, ...y} = x
  test.equal(foo, 123)
  test.equal(y, {gni="aabb"})
  test.equal(y?.foo, null)
  x = {foo=123, gni="aabb"}
  y = {bla=3.14, ...x}
  test.equal(y, {gni="aabb", foo=123, bla=3.14})
  x = 1.{foo=123, gni="aabb"}
  y = {bla=3.14, ...x}
  test.equal(y, {gni="aabb", foo=123, bla=3.14})

  # Make sure that a function that takes a record of the
  # type { foo?: int} can take a record of the type: {foo?: never}
  def f(x) =
    (x?.foo ?? 1) + 2
  end

  x = {foo=123}
  let {foo = _, ...y} = x
  test.equal(f(y), 3)

  # Make sure that we infer optional methods correctly
  def f(x) =
    ignore(x?.foo)
  end

  def g(x) =
    f(x.{gni=123})
  end

  g({blo="bla"})

  # Allow optional method extraction
  let {x?} = ()
  test.equal(x, null)

  let {x?} = {x=123}
  test.equal(x, 123)

  # The following is allowed:
  def json.parse =
    true
  end

  def json.parse.foo =
    false
  end

  def json.parse.gni(_) =
    true
  end

  let json.parse.foo = 456

  x = 1
  y = x.{foo=123} - 3
  test.equal("#{y}", "#{-2}")

  x = {foo=123}
  y = {gni="aabb"}
  test.equal({...x, ...y}, {gni="aabb", foo=123})
  test.equal({...x, gna=3.14, ...y}, {gni="aabb", gna=3.14, foo=123})
  test.equal(
    {...x, gna=3.14, ...y, foo="bar"}, {foo="bar", gni="aabb", gna=3.14}
  )

  x = {foo=1}
  y = {foo=2}
  test.equal({...x, ...y}, {foo=2})
  test.equal({...y, ...x}, {foo=1})

  x = {foo=1}
  y = {foo=null}
  test.equal({...x, ...y}, {foo=null})
  test.equal({...y, ...x}, {foo=1})

  x = {foo=null}
  y = {foo=null}
  test.equal({...x, ...y}, {foo=null})
  test.equal({...y, ...x}, {foo=null})

  x = {foo=1}
  y = {foo="a"}
  test.equal({...x, ...y}, {foo="a"})
  test.equal({...y, ...x}, {foo=1})

  x = {foo=1}
  y = {foo={bar=1}}
  test.equal({...x, ...y}, {foo={bar=1}})
  test.equal({...y, ...x}, {foo=1})

  # A required method that is a function returning `int?`
  def f(x) =
    x.foo() ?? 123
  end
  test.equal(f({foo=fun () -> null}), 123)

  def f(x) =
    x = (x : {foo: int})
    let {foo, ...x} = x
    ignore(foo)
    (x.foo ?? "aabb")
  end
  test.equal(f({foo=123}), "aabb")

  # Test priority for ?.
  x = ({} : {bla?: int})
  test.equal(x?.bla == x?.bla, true)

  # Test destructuring of record
  x = {foo={gni="aabb"}}
  foo = 123

  let {foo = {gni}} = x
  test.equal(foo, 123)
  test.equal(gni, "aabb")

  gni = 123
  let {foo = {gni = gno}, foo, foo = gna} = x
  test.equal(foo, {gni="aabb"})
  test.equal(gni, 123)
  test.equal(gno, "aabb")
  test.equal(gna, {gni="aabb"})

  x = {gni=123, bla=true}
  let {gni = _, ...rest} = x
  test.equal(rest, {bla=true})

  x = {gni={foo="aabb"}, bla="blo"}
  gni = 345
  let {gni = {foo}, gni = gno, ...rest} = x
  test.equal(gni, 345)
  test.equal(foo, "aabb")
  test.equal(gno, {foo="aabb"})
  test.equal(rest, {bla="blo"})

  test.pass()
end

test.check(f)
