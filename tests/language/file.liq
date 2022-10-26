#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

def f() =
  try
    ignore(file.read("mqlskjdfdjnsi"))
    test.fail()
  catch e : [error.file] do
    test.pass()
  end
end

test.check(f)
