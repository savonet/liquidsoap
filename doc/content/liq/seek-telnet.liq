# A playlist source
s = playlist("/path/to/music")

# The server seeking function
def seek(t) =
  t = float_of_string(default=0., t)
  log(
    "Seeking #{t} sec"
  )
  ret = source.seek(s, t)
  "Seeked #{ret} seconds."
end

# Register the function
server.register(
  namespace=source.id(s),
  description="Seek to a relative position in source #{source.id(s)}",
  usage="seek <duration>",
  "seek",
  seek
)
