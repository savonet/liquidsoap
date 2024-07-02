settings.init.allow_root := true
log.stdout := false

def f() =
  runtime.gc.compact()
  runtime.gc.compact()

  let {
    process_managed_memory,
    process_physical_memory,
    process_private_memory,
    process_swapped_memory
  } = runtime.memory().pretty

  print(
    "Physical memory: #{process_physical_memory}\nPrivate memory: #{
      process_private_memory
    }\nManaged memory: #{process_managed_memory}\nSwapped memory: #{
      process_swapped_memory
    }"
  )

  shutdown()
end

thread.run(delay=0.1, f)

output.dummy(blank())
