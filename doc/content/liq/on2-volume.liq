d = 1.
t = "lin"
s = fade.in(duration=d,type=t,
      fade.out(duration=d,type=t,sine(duration=3.*d)))
output.ao(visu.volume(s))
