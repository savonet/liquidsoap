#!../../src/liquidsoap ../../libs/pervasives.liq

# Testing tricky situation wrt channels. Because we have a function, the sineF
# source only knows that it is of type ('a,0,0). The fact that it has to be mono
# (because of the add) has to be determined by unification at execution time.

%include "test.liq"

set("log.level", 4)

first = ref(true)

def f()
  if !first then
    first := false
    [sine(id="sineF",880.)]
  else
    []
  end
end

s = mean(sine(id="sine"))
s = add([s , source.dynamic(f)])

output.dummy(s)

def kill() =
  test.pass()
  shutdown()
end
thread.run(delay=1., kill)
