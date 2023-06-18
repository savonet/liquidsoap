# Read the content of a file. Returns a function of type `()->string`. File is
# done reading when function returns the empty string `""`.
# @category File
def file.read(fname) =
  fd = file.open(write=false, fname)
  is_done = ref(false)
  def read() =
    if is_done() then
      ""
    else
      s = fd.read()
      is_done := s == ""
      if is_done() then
        fd.close()
      end
      s
    end
  end
  read
end

let file.write = ()

# Stream data to a file. Returns a callback to write to the file. Execute
# with `null` or `""` to signify the end of the writing operation.
# @category File
# @param ~append Append data if file exists.
# @param ~perms Default file rights if created. Default: `0o644`.
# @param path Path to write to
def file.write.stream(~perms=0o644, ~append=false, path) =
  fd = file.open(write=true, append=append, perms=perms, path)
  def write(s) =
    s = s ?? ""
    if s == "" then fd.close() else
      fd.write(s)
    end
  end
  write
end

# Write data to a file.
# @category File
# @param ~data Data to write. If passing a callback `() -> string?`, the callback \
#              must return `null` or `""` when it has finished sending all its data.
# @param ~append Append data if file exists.
# @param ~perms Default file rights if created. Default: `0o644`.
# @param path Path to write to
def replaces file.write(~data, ~perms=0o644, ~append=false, path) =
  cb = file.write.stream(append=append, perms=perms, path)
  def rec write() =
    s = getter.get(data) ?? ""
    cb(s)
    if getter.is_constant(data) then
      cb("")
    elsif s != "" then
      write()
    end
  end
  write()
end

# Read the whole contents of a file.
# @category File
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
# @category File
def file.lines(fname)
  r/\n/.split(file.contents(fname))
end

# Iterate over the lines of a file.
# @category File
def file.lines.iterator(fname)
  list.iterator(file.lines(fname))
end

# Iterate over the contents of a file.
# @category File
def file.iterator(fname)
  f = file.read(fname)
  fun () -> begin
    s = f()
    (s=="")?null():s
  end
end

# Get a file's mime type by calling the `file` command line binary.
# @category File
def file.mime.cli(fname) =
  mime = list.hd(default="",
    process.read.lines(
      process.quote.command("file", args=["-b", "--mime-type", fname])
    )
  )

  mime == "" ? null() : mime
end

# Get a file's mime type. Uses libmagic if enabled, otherwise try
# to get the value using the file binary. Returns `null()` if no value
# can be found.
# @category File
# @param file The file to test
def replaces file.mime(fname) =
  fn = file.mime.cli

%ifdef file.mime.libmagic
  ignore(fn)
  fn = file.mime.libmagic
%endif

  fn(fname)
end

# Getter to the contents of a file.
# @category File
# @param fname Name of the file from which the contents should be taken.
def file.getter(fname)
  contents = ref("")
  def update()
    contents := file.contents(fname)
  end
  update()
  ignore(file.watch(fname, update))
  ref.getter(contents)
end

# Float getter from a file.
# @category File
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

# Decode a flac-encoded cover metadata string.
# @category String
def file.metadata.flac.cover.decode(s) =
  # See https://xiph.org/flac/format.html#metadata_block_picture
  i = ref(0)

  def read_int() =
    ret = string.binary.to_int(little_endian=false, string.sub(s, start=i(), length=4))
    i := i() + 4
    ret
  end

  def read_string(len) =
    ret = string.sub(s, start=i(), length=len)
    i := i() + len
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
  data = string.sub(s, start=i(), length=data_len)

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

# Encode cover metadata for embedding with flac files.
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

# Download file using a regular http.get request. Returns `true` on success.
# @category File
# @param ~filename Downloaded filename.
# @param ~timeout Timeout in seconds
def file.download(~filename, ~timeout=5., url) =
  file_writer = file.write.stream(filename)

  response = http.get.stream(
    on_body_data=file_writer,
    timeout=timeout,
    url
  )

  response.status_code < 400
end
