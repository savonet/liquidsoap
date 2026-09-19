n = 8
starts = list.init(n, fun (_) -> ref(0.))
ends = list.init(n, fun (_) -> ref(0.))

# Each handler holds its slot for the whole pause, so if the last one to start
# did so before the first one finished, all of them were running at once.
def worker(i) =
  def run() =
    list.nth(starts, i) := time()
    thread.pause(0.4)
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
      first_end <= last_start
    then
      test.fail(
        "handlers did not overlap: last start #{last_start}, first end #{
          first_end
        }"
      )
    else
      test.pass()
    end
  end
end

thread.run(delay=3., check)
output.dummy(blank())
