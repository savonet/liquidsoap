log.level.set(4)

# A source built after startup gets its clock started. The script runs no other
# script code on purpose: nothing else would drain a clock left deferred.

def start_output() =
  o = output.dummy(fallible=true, sine())
  o.on_frame(synchronous=true, before=false, test.pass)
end

thread.run(delay=0.5, start_output)

# `test.fail` runs before this thread's own clock start, so it cannot mask it.
thread.run(delay=5., test.fail)
