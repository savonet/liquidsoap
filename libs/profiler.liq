# Enable profiling.
def profiler.enable()
  set("lang.profile", true)
end

# Print profiling information.
def profiler.print()
  print(profiler.stats_string())
end