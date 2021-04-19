#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

success = ref(false)
  
def f() =
  fname = "/tmp/fw"
  file.write(data="abc", fname)
  file.watch(fname, {success := true})
  file.write(data="xxx", fname)
  sleep(1.)
  
  if !success then test.pass() else test.fail() end
end

test.check(f)
