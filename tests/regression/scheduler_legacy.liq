# One legacy queue runs its tasks one after the other, so no two handlers may
# overlap. The domain pool would run all of them at once.
settings.scheduler.legacy := true
settings.scheduler.generic_queues := 1
settings.scheduler.fast_queues := 0
settings.scheduler.non_blocking_queues := 0

n = 3
starts = list.init(n, fun (_) -> ref(0.))
ends = list.init(n, fun (_) -> ref(0.))

def worker(i) =
  def run() =
    list.nth(starts, i) := time()
    thread.pause(0.3)
    list.nth(ends, i) := time()
  end

  thread.run(fast=false, run)
end

for i = 0 to n - 1 do worker(i)  end

def check() =
  started = list.map(fun (r) -> r(), starts)
  ended = list.map(fun (r) -> r(), ends)

  if
    list.exists(fun (t) -> t == 0., ended)
  then
    test.fail(
      "only #{list.length(list.filter(fun (t) -> t != 0., ended))} of #{n} \
       handlers finished"
    )
  else
    last_start = list.fold(fun (m, t) -> max(m, t), 0., started)
    first_end = list.fold(fun (m, t) -> min(m, t), list.hd(ended), ended)

    if
      last_start < first_end
    then
      test.fail(
        "handlers overlapped: last start #{last_start}, first end #{first_end}"
      )
    else
      test.pass()
    end
  end
end

thread.run(delay=3., check)
output.dummy(blank())
