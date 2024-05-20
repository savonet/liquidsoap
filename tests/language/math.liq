#!../../liquidsoap ../test.liq

def test_db_lin() =
  x = 5.

  test.almost_equal(dB_of_lin(lin_of_dB(x)), x)
  test.almost_equal(lin_of_dB(dB_of_lin(x)), x)

  y = -x
  test.equal(y, -5.)
  test.equal(y == -5., true)

  y = 1 + -2
  test.equal(y, -1)

  def f() =
    1 + -2
  end
  test.equal(f(), -1)

  test.equal(nan == nan, false)
  test.equal(nan != nan, true)

  test.equal(1e4, 10000.0)
  test.equal(1e+4, 10000.0)
  test.equal(1e-4, 0.0001)
  test.equal(1.1e-4, 0.00011)
  test.equal(1.e-4, 0.0001)

  test.pass()
end

test.check(test_db_lin)
