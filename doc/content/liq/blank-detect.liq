s = playlist("my-playlist")

# BEGIN
def handler() =
  process.run(
    "/path/to/your/script to do whatever you want"
  )
end
s = blank.detect(s)
s.on_blank(handler)

# END
output.dummy(fallible=true, s)
