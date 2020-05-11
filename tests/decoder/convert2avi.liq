#!../../src/liquidsoap ../../libs/pervasives.liq

# Intput file name
infile = argv(0)

# Check that it exists
if infile == "" then
  print("Error: please provide a file as argument.")
  exit(1)
elsif not file.exists(infile) then
  print("Error: file #{infile} does not exist.")
  exit(1)
end

# Output file name
outfile = argv(default=path.remove_extension(infile)^".avi", 1)

print(newline=false, "Encoding #{infile} to #{outfile}... ")

# Play the file once
s = once(single(infile))

# We use a clock with disabled synchronization
clock.assign_new(sync="none", [s])

# Function called at the end of encoding
def stop()
  print("done!")
  shutdown()
end

# Encode the file
output.file(%avi, outfile, fallible=true, on_stop=stop, s)
