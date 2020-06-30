#!../../src/liquidsoap ../../libs/pervasives.liq --force-start

%include "performance.liq"

n = 500000
l = list.init_right(n, fun(i) -> if i mod 2 == 0 then i/2 else n/2-i/2 end)

def compare (x,y)
  if x<y then -1 elsif x>y then 1 else 0 end
end

time("Sort list of size #{n}", {ignore(list.sort(compare, l))})

exit(0)
