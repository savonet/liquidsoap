# This function takes a metadata, checks if it is of type "file" and add the
# replay_gain metadata in this case.
def add_replaygain(m) =
  # Get the type
  type = m["type"]

  # The replaygain script is located there
  script = "#{configure.bindir}/extract-replaygain"

  # The file name is contained in this value
  filename = m["filename"]

  # If type = "song", proceed:
  if
    type == "song"
  then
    info =
      list.hd(
        process.read.lines(
          "#{script} #{filename}"
        )
      )
    [("replay_gain", info)]
    # Otherwise add nothing
  else
    []
  end
end
