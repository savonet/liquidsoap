s = playlist("my-playlist")
def handler() =
  process.run(
    "/path/to/your/script to do whatever you want"
  )
end
s = blank.detect(handler, s)
output.dummy(fallible=true, s)
