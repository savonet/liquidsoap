# Testing dynamic kind unification errors.

s = mean(sine())
def mkfilter(graph)
  x = ffmpeg.filter.audio.input(channels=2, graph, s)
  ffmpeg.filter.audio.output(graph, x)
end
s = ffmpeg.filter.create(mkfilter)
out(s)
