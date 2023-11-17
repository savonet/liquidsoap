protocol.add("beets", fun(~rlog=_, ~maxtime=_, arg) ->
  process.read.lines("/home/me/path/to/beet random -f '$path' #{arg}")
)
