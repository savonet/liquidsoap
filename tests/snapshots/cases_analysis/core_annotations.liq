# Sources, clocks and encoders come from the core: their types are computed
# here, and a bare source annotation carries no methods, as in liquidsoap.
def f((s : source_methods)) =
  s.skip()
end
c = (sine().clock : clock)
o = output.file(%mp3, "/tmp/out.mp3", sine())
ignore((f, c, o))

#? type 4:2
#? type 6:0
