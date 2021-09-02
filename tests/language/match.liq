#!../../src/liquidsoap ../../libs/stdlib.liq

%include "test.liq"
  
success = ref(true)

def t(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end
  
def f() =
  # Simple test
  x = match 2
    case x do 4
  end

  # Two ground types
  def f(x) =
    match x
      case x : string do
        ignore (x^"")
        4
      case y : int do
        y+2
    end
  end
  
  if !success then test.pass() else test.fail() end
end

test.check(f)
