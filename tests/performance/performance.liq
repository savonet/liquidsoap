def time(name, f)
  print("Computing #{name}...")
  t = gettimeofday()
  f()
  t' = gettimeofday()
  print("Computing #{name} took #{t'-t} seconds.")
end
