profile = (environment.get("LIQ_PROFILE") != "")

def time(name, f) =
  if profile then profiler.enable() end
  print(
    "Computing #{name}..."
  )
  t = time()
  f()
  t = time() - t
  print(
    "Computing #{name} took #{string.float(decimal_places=2, t)} seconds."
  )
  test.metric(category="performance", name=name, value=t, unit="s", min=0.)
  if profile then print(profiler.stats.string()) end
end
