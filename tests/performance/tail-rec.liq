#!../../src/liquidsoap ../../libs/pervasives.liq --force-start

%include "performance.liq"

m = 100000

def rec f(n)
  if n < m then f(n+1) end
end

time("#{m} recursive calls", {f(0)})

exit(0)
