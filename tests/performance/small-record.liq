#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq --force-start

%include "performance.liq"

def r10()
  r = {a0 = 0, a1 = 1, a2 = 2, a3 = 3, a4 = 4, a5 = 5, a6 = 6, a7 = 7, a8 = 8, a9 = 9}

  x = ref(0)

  for _ = 1 to 200000 do
    x := !x + r.a0 + r.a1 + r.a2 + r.a3 + r.a4 + r.a5 + r.a6 + r.a7 + r.a8 + r.a9
  end
end

time("record with 10 fields", r10)

exit(0)
