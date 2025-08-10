# The input file, any format supported by liquidsoap
input = "/tmp/input.mp3"

# The output file
target = "/tmp/output.ogg"

# A source that plays the file once
source = once(single(input))

# We use a clock with disabled synchronization
clock.assign_new(sync="none", [source])

# Finally, we output the source to an ogg/vorbis file
o = output.file(%vorbis, target, fallible=true, source)

o.on_stop(synchronous=true, shutdown)
