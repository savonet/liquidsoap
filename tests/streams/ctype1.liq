# Testing tricky situation wrt channels. Because we have a function, the sineF
# source only knows that it is of type ('a,0,0). The fact that it has to be mono
# (because of the add) has to be determined by unification at execution time.

log.level.set(4)

def f()
  sine(id="sineF",880.)
end

s = mean(sine(id="sine"))
s = add([s , f()])

output.dummy(s)
thread.run(delay=1., {test.pass(); shutdown()})
