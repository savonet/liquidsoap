#!../../liquidsoap ../test.liq

def f() =
  mu = runtime.memory()
  print(
    "Memory usage:\n#{mu}"
  )
  test.metric(
    category="memory",
    name="Memory usage: process private memory",
    value=float(mu.process_private_memory) / (1024. * 1024.),
    unit="MiB",
    min=0.
  )

  test.metric(
    category="memory",
    name="Memory usage: process swapped memory",
    value=float(mu.process_swapped_memory) / (1024. * 1024.),
    unit="MiB",
    min=0.
  )

  test.metric(
    category="memory",
    name="Memory usage: process physical memory",
    value=float(mu.process_physical_memory) / (1024. * 1024.),
    unit="MiB",
    min=0.
  )

  test.metric(
    category="memory",
    name="Memory usage: process virtual memory",
    value=float(mu.process_virtual_memory) / (1024. * 1024.),
    unit="MiB",
    min=0.
  )

  test.metric(
    category="memory",
    name="Memory usage: total physical memory",
    value=float(mu.total_physical_memory) / (1024. * 1024.),
    unit="MiB",
    min=0.
  )

  test.metric(
    category="memory",
    name="Memory usage: total virtual memory",
    value=float(mu.total_virtual_memory) / (1024. * 1024.),
    unit="MiB",
    min=0.
  )

  test.metric(
    category="memory",
    name="Memory usage: total used physical memory",
    value=float(mu.total_used_physical_memory) / (1024. * 1024.),
    unit="MiB",
    min=0.
  )

  test.metric(
    category="memory",
    name="Memory usage: total used virtual memory",
    value=float(mu.total_used_virtual_memory) / (1024. * 1024.),
    unit="MiB",
    min=0.
  )

  # See #2427
  thread.run({ignore(runtime.memory())})
  test.pass()
end

test.check(f)
