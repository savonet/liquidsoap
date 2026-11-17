%include "../test.liq"

# Requests are created from every clock's thread: the shared pool must stay
# consistent when they do so at the same time.

workers = 4
per_worker = 20000
done_count = ref(0)
active = ref(0)
max_active = ref(0)

def worker() =
  ref.incr(active)
  max_active := max(max_active(), active())
  for _ = 1 to per_worker do request.destroy(request.create("foo"))  end
  active := active() - 1
  ref.incr(done_count)
end

for _ = 1 to workers do thread.run(fast=true, worker)  end

def check() =
  if
    done_count() < workers
  then
    0.1
  else
    # The race needs workers running at the same time.
    if
      max_active() > 1
    then
      test.pass()
    else
      test.fail(
        "workers never ran concurrently"
      )
    end
    (-1.)
  end
end

thread.run.recurrent(check)
