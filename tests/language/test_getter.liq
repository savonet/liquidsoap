#!../../liquidsoap ../test.liq

# Test getters, see #1181
def f(~x) =
  ignore(getter.get(x) + 2)
end

def g(~x=getter(2)) =
  x = getter.function(x)
  ignore(x() + 2)
end

def tests() =
  f(x=3)
  f(x={4})
  g()
  g(x=3)
  g(x={4})
  r = ref(5)
  f(x=r)
  g(x=r)
  r.set(r() + 2)
  test.equal(getter.get(getter(3)), 3)
  test.equal(getter.get(getter({3})), 3)
  test.equal(getter.get(getter.map(fun (x) -> 2 * x, getter(3))), 6)
  test.equal(getter.get(getter.map(fun (x) -> 2 * x, getter({3}))), 6)
  test.equal(getter.is_constant(getter(3)), true)
  test.equal(getter.is_constant(getter({3})), false)

  def gen =
    pos = ref(0)
    fun () ->
      begin
        if
          pos() == 3
        then
          ""
        else
          ref.incr(pos)
          "foobar"
        end
      end
  end

  test.equal(
    string.getter.flush(separator=",", getter(gen)),
    "foobar,foobar,foobar"
  )

  test.equal(string.getter.flush(getter("foobar")), "foobar")

  def gen =
    pos = ref(0)
    fun () ->
      begin
        if
          pos() == 3
        then
          ""
        else
          ref.incr(pos)
          "foobar"
        end
      end
  end

  test.equal(
    string.getter.flush(
      separator="|",
      string.getter.concat([getter(""), getter(gen), getter("gno")])
    ),
    "foobar|foobar|foobar|gno"
  )

  # Check that memoize does memoize!
  count = ref(0)
  def f(_) =
    ref.incr(count)
    if count() > 1 then test.fail() end
  end
  g = getter("aabb")
  g = getter.map.memoize(f, g)
  getter.get(g)
  getter.get(g)

  test.pass()
end

test.check(tests)
