# BEGIN
# This function is called when a new metadata block is passed in the stream.
def apply_metadata(m) =
  title = m["title"]
  artist = m["artist"]
  print("Now playing: #{title} by #{artist}")
end
# END
s = sine()
# BEGIN2
s = source.on_metadata(s, apply_metadata)
# END2
output.dummy(fallible=true, s)
