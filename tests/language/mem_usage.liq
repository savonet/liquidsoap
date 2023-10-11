#!../../liquidsoap ../test.liq
success = ref(true)

def f() =
  mu = runtime.mem_usage()
  print(
    "Memory usage:\n#{mu}"
  )
  test.metric(
    category="memory",
    name=
      "Memory usage: process physical memory",
    value=float(mu.process_physical_memory) / (1024. * 1024.),
    unit="Mio",
    min=0.
  )

  test.metric(
    category="memory",
    name=
      "Memory usage: process virtual memory",
    value=float(mu.process_virtual_memory) / (1024. * 1024.),
    unit="Mio",
    min=0.
  )

  test.metric(
    category="memory",
    name=
      "Memory usage: total physical memory",
    value=float(mu.total_physical_memory) / (1024. * 1024.),
    unit="Mio",
    min=0.
  )

  test.metric(
    category="memory",
    name=
      "Memory usage: total virtual memory",
    value=float(mu.total_virtual_memory) / (1024. * 1024.),
    unit="Mio",
    min=0.
  )

  test.metric(
    category="memory",
    name=
      "Memory usage: total used physical memory",
    value=float(mu.total_used_physical_memory) / (1024. * 1024.),
    unit="Mio",
    min=0.
  )

  test.metric(
    category="memory",
    name=
      "Memory usage: total used virtual memory",
    value=float(mu.total_used_virtual_memory) / (1024. * 1024.),
    unit="Mio",
    min=0.
  )

  # See #2427
  thread.run({ignore(runtime.mem_usage())})
  test.pass()
end

test.check(f)
