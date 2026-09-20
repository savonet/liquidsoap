# Encoders are implemented by the core, which the analysis does not link.
bitrate = 128
o = output.file(%mp3(bitrate=bitrate), "/tmp/out.mp3", sine())
ignore(o)

#? type 2:0
#? type 3:0
