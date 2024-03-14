#!../../liquidsoap ../test.liq

clock.autostart := false

def test_db_lin() =
  x = 5.

  test.almost_equals(dB_of_lin(lin_of_dB(x)), x)
  test.almost_equals(lin_of_dB(dB_of_lin(x)), x)

  y = -x
  test.equals(y, -5.)
  test.equals(y == -5., true)

  y = 1 + -2
  test.equals(y, -1)

  def f() =
    1 + -2
  end
  test.equals(f(), -1)

  test.equals(nan == nan, false)
  test.equals(nan != nan, true)

  test.pass()
end

test.check(test_db_lin)
