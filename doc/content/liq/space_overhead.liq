#!./liquidsoap
# Collect more often: less memory, more CPU.
runtime.gc.set(runtime.gc.get().{space_overhead = 40})
