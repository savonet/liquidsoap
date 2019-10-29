#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

success = ref(true)

def test(x, y)
  if x != y then
    print("Failure: got #{x} instead of #{y}")
    success := false
  end
end

f = ?
def g(c) =
  def h(x) = if x != 3 then success := false end; 5 end
  command.set(c,h);
  2  
end
test(g(f), 2)
test(f(3), 5)

if !success then test.pass() else test.fail() end
