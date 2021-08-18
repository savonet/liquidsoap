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
  # Test strictness and evaluation order
  # l = ref(false)
  # r = ref(false)
  # if begin l := true; true end or begin r := true; true end then () end
  # print("#{!l} / #{!r}")
  # t(!l and (not !r), true)

  #1820
  ignore(true == false ? 5 : 6)
end

test.check(f)
