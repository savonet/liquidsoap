# Error raised when a problem occurred when accessing files.
# @category System
let file.error = error.register("file")

# Read the whole contents of a file.
# @category System
def file.contents(fname) =
  fn = file.read(fname)
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

# Get the list of lines of a file.
# @category System
def file.lines(fname)
  string.split(separator="\n", file.contents(fname))
end

# Iterate over the lines of a file.
# @category System
def file.lines.iterator(fname)
  list.iterator(file.lines(fname))
end

# Iterate over the contents of a file.
# @category System
def file.iterator(fname)
  f = file.read(fname)
  fun () -> begin
    s = f()
    (s=="")?null():s
  end
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
      list.hd(default="", process.read.lines("file -b --mime-type #{process.quote(fname)}"))
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

# Getter to the contents of a file.
# @category System
# @param fname Name of the file from which the contents should be taken.
def file.getter(fname)
  contents = ref("")
  def update()
    contents := file.contents(fname)
  end
  update()
  ignore(file.watch(fname, update))
  {!contents}
end

# Float getter from a file.
# @category System
# @param fname Name of the file from which the contents should be taken.
# @param ~default Default value when the file contains invalid data.
def file.getter.float(~default=0., fname)
  x = file.getter(fname)
  def f(x) = float_of_string(default=default, string.trim(x)) end
  getter.map.memoize(f, x)
end

%ifndef file.metadata.flac
let file.metadata.flac = fun (_) -> []
%endif

let file.metadata.flac.cover = ()

# Decode a flac-encoded cover metadata string
# @category String
def file.metadata.flac.cover.decode(s) =
  # See https://xiph.org/flac/format.html#metadata_block_picture
  i = ref(0)

  def read_int() =
    ret = string.binary.to_int(little_endian=false, string.sub(s, start=!i, length=4))
    i := !i + 4
    ret
  end

  def read_string(len) =
    ret = string.sub(s, start=!i, length=len)
    i := !i + len
    (ret:string)
  end

  pic_type = read_int()
  mime_len = read_int()
  mime = mime_len == 0 ? "image/" : read_string(mime_len)
  desc_len = read_int()
  desc = read_string(desc_len)
  width = read_int()
  height = read_int()
  color_depth = read_int()
  number_of_colors = read_int()
  number_of_colors = number_of_colors > 0 ? null(number_of_colors) : null()
  data_len = read_int()
  data = string.sub(s, start=!i, length=data_len)

  if data == "" then
    log.info("Failed to read cover metadata")
    null()
  else
    null(data.{
      picture_type=pic_type,
      mime=mime, description=desc,
      width=width, height=height,
      color_depth=color_depth,
      number_of_colors=number_of_colors
     })
  end
end

# Encode cover metadata for embedding with 
# flac files
# @category String
def file.metadata.flac.cover.encode(
        ~picture_type,
        ~mime, ~description="",
        ~width, ~height,
        ~color_depth, ~number_of_colors=null(),
        data) =
  def encode_string(s) =
    len = 1 + (string.length(s) / 8)
    str_len = string.binary.of_int(little_endian=false, pad=4, len)
    if string.length(str_len) > 4 then
      error.raise(error.invalid, "Data length too long for APIC format!")
    end
    pad = string.make(char_code=0, len * 8 - string.length(s))
    (str_len, "#{s}#{pad}") 
  end
  pic_type = string.binary.of_int(little_endian=false, pad=4, picture_type)
  let (mime_len, mime) = encode_string(mime)
  let (desc_len, description) = encode_string(description)
  width = string.binary.of_int(little_endian=false, pad=4, width)
  height = string.binary.of_int(little_endian=false, pad=4, height)
  color_depth = string.binary.of_int(little_endian=false, pad=4, color_depth)
  number_of_colors = string.binary.of_int(little_endian=false, pad=4, number_of_colors ?? 0)
  let (data_len, data) = encode_string(data)
  "#{pic_type}#{mime_len}#{mime}#{desc_len}#{description}#{width}#{height}#{color_depth}#{number_of_colors}#{data_len}#{data}"
end
