def scratch_file() =
  scratch = file.temp("scratch", ".txt")

  def write_line(s) =
    file.write(append=true, data=s, scratch)
  end

  runtime.gc.finalise.last({file.remove(scratch)}, write_line)
  write_line
end

notes = scratch_file()
notes(
  "first line\n"
)
