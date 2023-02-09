#!../../liquidsoap ../test.liq

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    test.fail()
  end
end

def incorrect(expr)
  print("Incorrect expression #{expr}...\n")

  try
    let eval _ = expr
    test.fail()
  catch err do
    print("Got err: #{err}")
  end

  print("\n")
end

def f() =
  def double(x) = 2. * x end

  t(double(3), 6.)

  ignore(if false then 1. else 2 end)

  incorrect("if false then 1 else 2. end")

  def f(l) =
    l = [1, ...l]
    l = [2., ...l]
    l
  end

  incorrect("
  def f(l) =
    l = [2., ...l]
    l = [1, ...l]
    l
  end
  ")

  def f(l) =
    let [x] = l
    ignore(x + 2.)
    let [x] = l
    ignore(x + 1)
  end

  incorrect("
  def f(l) =
    let [x] = l
    ignore(x + 1)
    let [x] = l
    ignore(x + 2.)
  end
  ")

  test.pass()
end

test.check(f)
