# BEGIN
# This function updates the title metadata with the content of "/tmp/metadata".
def update_title(m) =
  # The title metadata
  title = m["title"]

  # Our addition
  content =
    list.hd(
      process.read.lines(
        "cat /tmp/metadata"
      )
    )

  # If title is empty
  if
    title == ""
  then
    [("title", content)]
    # Otherwise
  else
    [
      (
        "title",
        "#{title} on #{content}"
      )
    ]
  end
end

# END
s = sine()

# BEGIN2
s = metadata.map(update_title, s)

# END2
output.dummy(fallible=true, s)
