#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

def f() =
  def f(~(foo:int), ~(bar:string)="123", (x:source), (y:int)=123) =
    ()
  end

  test.pass()
end

test.check(f)
