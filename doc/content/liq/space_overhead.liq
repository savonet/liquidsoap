#!./liquidsoap
# This code was contributed by AzuraCast. Possible settings:
# - less memory: space_overhead = 20
# - less cpu:    space_overhead = 140
# - balanced:    space_overhead = 80

# Optimize for memory usage over CPU: this results in a slightly increased CPU
# usage and reduced memory usage.
runtime.gc.set(runtime.gc.get().{space_overhead=20, allocation_policy=2})
