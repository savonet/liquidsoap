#!../../src/liquidsoap ../../libs/pervasives.liq

set("log.level", 4)

infile = argv(0)
if infile == "" then
  print("Error: please provide a file as argument.")
  exit(1)
elsif not file.exists(infile) then
  print("Error: file #{infile} does not exist.")
  exit(1)
end

s = drop_audio(single(infile))
output.sdl(s)