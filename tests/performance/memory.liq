#!/usr/bin/env -S ../../liquidsoap ../test.liq performance.liq

m = runtime.memory().process_managed_memory
print(
  "Managed memory: #{m} octets"
)
test.metric(
  category="memory",
  name="Process managed memory",
  value=float(m) / (1024. * 1024.),
  unit="Mio",
  min=0.
)
test.pass()
