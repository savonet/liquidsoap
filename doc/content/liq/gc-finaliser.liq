def scratch_file() =
  scratch = file.temp("scratch", ".txt")

  def write_line(s) =
    file.write(append=true, data=s, scratch)
  end

  runtime.gc.finalise.last({file.remove(scratch)}, write_line)
  write_line
end

# The writer is dropped when the function returns: the scratch file is
# removed some time after.
def take_notes() =
  notes = scratch_file()
  notes(
    "first line\n"
  )
end

take_notes()
