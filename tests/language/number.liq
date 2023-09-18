def f() =
  test.equals(is_nan(nan), true)
  test.equals(is_nan(1.), false)
  test.equals(is_nan(infinity), false)
  test.equals(is_infinite(nan), false)
  test.equals(is_infinite(1.), false)
  test.equals(is_infinite(infinity), true)
  test.equals(is_infinite(-(infinity)), true)
  test.equals(int(nan), 0)
  try
    ignore(int(raise=true, nan))
    test.fail()
  catch _ : [error.math] do
    ()
  end

  test.equals(int(infinity), max_int)
  test.equals(int(-(infinity)), min_int)
  try
    ignore(int(raise=true, infinity))
    test.fail()
  catch _ : [error.math] do
    ()
  end

  try
    ignore(int(raise=true, -(infinity)))
    test.fail()
  catch _ : [error.math] do
    ()
  end

  test.equals(ceil(1.), 1.)
  test.equals(ceil(1.4), 2.)
  test.equals(ceil(1.5), 2.)
  test.equals(ceil(1.6), 2.)
  test.equals(ceil(0.1), 1.)
  test.equals(ceil(0.), 0.)
  test.equals(ceil(-0.1), -0.)
  test.equals(ceil(-1.), -1.)
  test.equals(ceil(-1.4), -1.)
  test.equals(ceil(-1.5), -1.)
  test.equals(ceil(-1.6), -1.)
  test.equals(ceil(nan), nan)
  test.equals(floor(1.), 1.)
  test.equals(floor(1.4), 1.)
  test.equals(floor(1.5), 1.)
  test.equals(floor(1.6), 1.)
  test.equals(floor(0.1), 0.)
  test.equals(floor(0.), 0.)
  test.equals(floor(-0.1), -1.)
  test.equals(floor(-1.), -1.)
  test.equals(floor(-1.4), -2.)
  test.equals(floor(-1.5), -2.)
  test.equals(floor(-1.6), -2.)
  test.equals(floor(nan), nan)
  test.equals(sign(0.), 1.)
  test.equals(sign(0.1), 1.)
  test.equals(sign(1.), 1.)
  test.equals(sign(2.), 1.)
  test.equals(sign(-0.1), -1.)
  test.equals(sign(-1.), -1.)
  test.equals(sign(-2.), -1.)
  test.equals(sign(nan), 1.)
  test.equals(round(0.), 0.)
  test.equals(round(infinity), infinity)
  test.equals(round(nan), nan)
  test.pass()
end

test.check(f)
