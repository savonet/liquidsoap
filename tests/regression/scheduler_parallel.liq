%include "../test.liq"

n = 8
arrived = ref(0)
met = ref(0)

# Each handler waits at a barrier until all of them have arrived, so they can
# only all get through if they run at once, however late each one starts.
def worker(_) =
  def run() =
    atomic({ref.incr(arrived)})
    deadline = time() + 5.
    while arrived() < n and time() < deadline do thread.pause(0.01)  end
    if arrived() >= n then atomic({ref.incr(met)}) end
  end

  thread.run(fast=false, run)
end

for i = 0 to n - 1 do worker(i)  end

give_up = time() + 10.

def rec check() =
  if
    met() == n
  then
    test.pass()
  elsif
    time() > give_up
  then
    test.fail(
      "only #{met()} of #{n} handlers ran at once (#{arrived()} arrived)"
    )
  else
    thread.run(delay=0.1, check)
  end
end

thread.run(delay=0.1, check)
output.dummy(blank())
