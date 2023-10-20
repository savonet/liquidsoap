#!../../liquidsoap ../test.liq

def test_db_lin() =
  x = 5.

  test.almost_equals(dB_of_lin(lin_of_dB(x)), x)
  test.almost_equals(lin_of_dB(dB_of_lin(x)), x)

  test.pass()
end

test.check(test_db_lin)
