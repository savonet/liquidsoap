#!../../liquidsoap ../test.liq

# Test getters, see #1181

def f(~x)
  ignore(getter.get(x) + 2)
end

def g(~x=getter(2))
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
  r.set(r()+2)

  test.equals(getter.get(getter(3)), 3)
  test.equals(getter.get(getter({3})), 3)
  test.equals(getter.get(getter.map(fun (x) -> 2*x, getter(3))), 6)
  test.equals(getter.get(getter.map(fun (x) -> 2*x, getter({3}))), 6)
  test.equals(getter.is_constant(getter(3)), true)
  test.equals(getter.is_constant(getter({3})), false)

  def gen =
    pos = ref(0)
    fun () -> begin
      if pos() == 3 then "" else
        ref.incr(pos)
        "foobar"
      end
    end
  end

  test.equals(string.getter.flush(separator=",", getter(gen)), "foobar,foobar,foobar")
  test.equals(string.getter.flush(getter("foobar")), "foobar")

  def gen =
    pos = ref(0)
    fun () -> begin
      if pos() == 3 then "" else
        ref.incr(pos)
        "foobar"
      end
    end
  end

  test.equals(string.getter.flush(separator="|", string.getter.concat([getter(""), getter(gen), getter("gno")])), "foobar|foobar|foobar|gno")

  test.pass()
end

test.check(tests)
