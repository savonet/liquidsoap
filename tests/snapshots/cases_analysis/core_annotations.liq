# Sources, clocks and encoders come from the core, which the analysis does not
# link: their types come from the typing environment.
def f((s : source)) =
  s.skip()
end
c = (sine().clock : clock)
o = output.file(%mp3, "/tmp/out.mp3", sine())
ignore((f, c, o))

#? methods 4:2
#? type 4:2
#? type 6:0
