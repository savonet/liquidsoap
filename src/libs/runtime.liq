# Returns information about the system and process' memory
# usage. Requires `mem_usage` for a full report.
# @category Liquidsoap
def runtime.memory() =
  x = {
    process_managed_memory = runtime.gc.quick_stat().heap_words * runtime.sys.word_size / 8
  }

%ifdef runtime.mem_usage
  info = runtime.mem_usage()
  x = x.{
    total_virtual_memory = info.total_virtual_memory,
    total_physical_memory = info.total_physical_memory,
    total_used_virtual_memory = info.total_used_virtual_memory,
    total_used_physical_memory = info.total_used_physical_memory,
    process_virtual_memory = info.process_virtual_memory,
    process_physical_memory = info.process_physical_memory
  }

  let x.pretty = {
    process_managed_memory = runtime.mem_usage.prettify_bytes(x.process_managed_memory),
    total_virtual_memory = runtime.mem_usage.prettify_bytes(info.total_virtual_memory),
    total_physical_memory = runtime.mem_usage.prettify_bytes(info.total_physical_memory),
    total_used_virtual_memory = runtime.mem_usage.prettify_bytes(info.total_used_virtual_memory),
    total_used_physical_memory = runtime.mem_usage.prettify_bytes(info.total_used_physical_memory),
    process_virtual_memory = runtime.mem_usage.prettify_bytes(info.process_virtual_memory),
    process_physical_memory = runtime.mem_usage.prettify_bytes(info.process_physical_memory)
  }
%endif

  x
end

let runtime.cpu = ()
%ifdef process.time
# Create a function returning CPU usage (in `float` percent so
# `0.2` means `20%`) since the last time it was called.
# @category Liquidsoap
def runtime.cpu.usage_getter() =
  t = ref(time())
  let {user, system} = process.time()
  u = ref(user)
  s = ref(system)
  def f() =
    t' = time()
    delta = t' - t()

    let {user, system} = process.time()
    u' = user - u()
    s' = system - s()

    t := t'
    u := user
    s := system

    { user = u' / delta, system = s' / delta, total = (u'+s') / delta }
  end

  f
end
%endif

# Set the current time zone. This is
# equivalent to setting the `TZ` environment
# variable.
# @category Time
def time.zone.set(tz) =
  environment.set("TZ", tz)
end
