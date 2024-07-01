settings.init.allow_root := true
log.stdout := false

def cleanup() =
  runtime.gc.full_major()
  runtime.gc.compact()
end

def print() =
  let {
    process_managed_memory,
    process_physical_memory,
    process_private_memory
  } = runtime.memory()

  print(json.stringify(runtime.memory()))

  shutdown()
end

thread.run(delay=1., cleanup)
thread.run(delay=2., print)

output.dummy(blank())
