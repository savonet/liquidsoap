s = request.dynamic(
  {
    request.create("bar:foo", indicators = process.read.lines("cat " ^ process.quote("playlist.pls")))
  }
)
# END
output.dummy(fallible=true, s)
