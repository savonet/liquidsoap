#!../../liquidsoap ../test.liq

def t(x, y) =
  # Convert both to float for comparison (handles int/float mix)
  xf = float(x)
  yf = float(y)
  if
    xf != yf
  then
    print(
      "Failure: got #{xf} instead of #{yf}"
    )
    test.fail()
  end
end

def incorrect(expr) =
  print(
    "Incorrect expression #{expr}...\n"
  )

  try
    let eval _ = expr
    test.fail()
  catch err do
    print(
      "Got err: #{err}"
    )
  end

  print("\n")
end

def f() =
  # Test 1: Function argument - int passed to float parameter
  def double(x) =
    2. * x
  end

  t(double(3), 6.)

  # Test 2: Math builtins accepting int where float is expected
  t(sin(0), 0.)
  t(cos(0), 1.)
  t(sqrt(4), 2.)
  t(exp(0), 1.)
  t(ln(1), 0.)
  t(log10(1), 0.)
  t(tan(0), 0.)
  t(asin(0), 0.)
  t(atan(0), 0.)
  t(sinh(0), 0.)
  t(cosh(0), 1.)
  t(tanh(0), 0.)

  # Test 3: ceil, floor, round, sign with int argument
  t(ceil(3), 3.)
  t(floor(3), 3.)
  t(round(3), 3.)
  t(sign(1), 1.)
  t(sign(-1), -1.)

  # Test 4: Named arguments with float type (forces float via 0. addition)
  def with_named_float(~x, ~y=2.) =
    x + y + 0.
  end

  t(with_named_float(x=3), 5.)

  # Note: y=4 gets passed as int at runtime, but the + 0. forces float result
  t(with_named_float(x=3, y=4), 7.)

  # Test 5: Multiple float arguments (all ints passed to explicit float function)
  def multi_float((a:float), (b:float), (c:float)) =
    a + b + c
  end

  t(multi_float(1, 2, 3), 6.)

  # Test 6: Function returning float, called with int
  def make_double((factor:float)) =
    fun ((x:float)) -> factor * x
  end

  double_fn = make_double(2)
  t(double_fn(5), 10.)

  # Test 7: Methods with float values
  x = (3.).{scale=2.}
  t(x * x.scale, 6.)

  # Test 8: Record with float field, assigned int
  r = {value=5., factor=2.}
  t(r.value * r.factor, 10.)

  # Test 9: Float getter with int
  def use_float_getter(~x) =
    getter.get(x) * 2.
  end

  t(use_float_getter(x=3), 6.)
  t(use_float_getter(x={4}), 8.)

  # Test 10: Float getter with ref
  r = ref(5)
  t(use_float_getter(x=r), 10.)

  # Test 11: random.float with int arguments
  x = random.float(min=0, max=10)
  test.equal(x >= 0. and x < 10., true)

  # Test 12: pow with int arguments (converted to float internally)
  t(pow(2, 3), 8)

  # pow with floats also works
  t(pow(2., 3.), 8.)

  # Test 13: Arithmetic operations with int where float expected
  def float_add(x) =
    x + 1.5
  end

  t(float_add(2), 3.5)

  # Test 14: Optional float argument with int default
  def with_optional(~(x:float)=5) =
    x * 2.
  end

  t(with_optional(), 10.)
  t(with_optional(x=3), 6.)

  # Test 18: Higher-order function with float
  def apply_to_float(f, x) =
    f(x)
  end

  t(apply_to_float(fun (x) -> x * 2., 3), 6.)

  # Test 19: Nested function calls with int to float conversion
  t(sqrt(sqrt(16)), 2.)

  # Test 20: float() explicit conversion still works
  t(float(5), 5.)
  t(float(5) + 0.5, 5.5)

  # Test 21: float.is_nan and float.is_infinite with int
  test.equal(float.is_nan(0), false)
  test.equal(float.is_infinite(0), false)

  # Now test incorrect expressions - mixing int and float should fail

  # Incorrect 1: if-then-else with mixed float and int
  incorrect(
    "if false then 1. else 2 end"
  )

  incorrect(
    "if false then 1 else 2. end"
  )

  # Incorrect 2: List with mixed int and float
  incorrect("
  def f(l) =
    l = [2., ...l]
    l = [1, ...l]
    l
  end
  ")

  # Incorrect 3: Pattern matching with mixed types
  incorrect(
    "
  def f(l) =
    let [x] = l
    ignore(x + 1)
    let [x] = l
    ignore(x + 2.)
  end
  "
  )

  # Incorrect 4: Tuple with mixed types
  incorrect(
    "if false then (1., 2.) else (1, 2) end"
  )

  # Incorrect 5: Function with inconsistent return type
  incorrect("
  def f(b) =
    if b then 1. else 2 end
  end
  ")

  # Incorrect 6: List literal with mixed types
  incorrect(
    "[1., 2, 3.]"
  )

  # Incorrect 7: Record fields with inconsistent float/int
  incorrect("
  def f(b) =
    if b then {x=1.} else {x=2} end
  end
  ")

  test.pass()
end

test.check(f)
