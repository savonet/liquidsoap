%include "../test.liq"

# Requests are created from every clock's thread: the shared pool must stay
# consistent when they do so at the same time.

workers = 4
per_worker = 20000
done_count = ref(0)

def worker() =
  for _ = 1 to per_worker do request.destroy(request.create("foo"))  end
  ref.incr(done_count)
end

for _ = 1 to workers do thread.run(fast=true, worker)  end

def check() =
  if
    done_count() == workers
  then
    test.pass()
    (-1.)
  else
    0.1
  end
end

thread.run.recurrent(check)
