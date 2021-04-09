#!../..//src/liquidsoap ../../libs/main.liq

%include "../test.liq"

def fn() =
  a = ()

  # My function
  # @param ~x The parameter.
  def a.b(~x)
    0
  end

  ignore(a)

  test.pass()
end

test.check(fn)
