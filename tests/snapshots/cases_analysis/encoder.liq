# The core builds encoders; their types are the analysis' own.
bitrate = 128
o = output.file(%mp3(bitrate=bitrate), "/tmp/out.mp3", sine())
ignore(o)

#? type 2:0
#? type 3:0
#? type 3:16
