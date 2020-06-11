profile = (getenv("LIQ_PROFILE") != "")

def time(name, f)
  if profile then profiler.enable() end
  print("Computing #{name}...")
  t = gettimeofday()
  f()
  t' = gettimeofday()
  print("Computing #{name} took #{t'-t} seconds.")
  if profile then print(profiler.stats.string()) end
end
