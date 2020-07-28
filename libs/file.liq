# Error raised when a problem occurred when accessing files.
# @category System
let file.error = error.register("file")

# Read the whole contents of a file.
# @category System
def file.contents(f) =
  fn = file.read(f)
  def rec f(cur) =
    s = fn()
    if s == "" then
      cur
    else
      f("#{cur}#{s}")
    end
  end
  f("")
end

# Dummy implementation of file.mime
# @category System
def file.mime_default(_)
  ""
end
%ifdef file.mime
# Alias of file.mime (because it is available)
# @category System
def file.mime_default(fname)
  file.mime(fname)
end
%endif

# Generic mime test. First try to use file.mime if it exist.  Otherwise try to
# get the value using the file binary. Returns "" (empty string) if no value
# can be found.
# @category System
# @param file The file to test
def get_mime(fname) =
  def file_method(fname) =
    if process.test("which file") then
      list.hd(default="", process.read.lines("file -b --mime-type #{string.quote(fname)}"))
    else
      ""
    end
  end
  # First try mime method
  ret = file.mime_default(fname)
  if ret != "" then
    ret
  else
    # Now try file method
    file_method(fname)
  end
end
