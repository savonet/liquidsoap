protocol.add(
  "beets",
  fun (~rlog:_, ~maxtime:_, arg) ->
    list.hd(
      process.read.lines(
        "/home/me/path/to/beet random -f '$path' #{arg}"
      )
    )
)
