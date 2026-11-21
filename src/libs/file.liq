# @docof file.temp
# @param ~cleanup Delete the file on shutdown
def file.temp(~cleanup=false, %argsof(file.temp), prefix, suffix) =
  f = file.temp(%argsof(file.temp), prefix, suffix)
  if cleanup then on_cleanup({file.remove(f)}) end
  f
end

# @docof file.temp_dir
# @param ~cleanup Delete the file on shutdown
def file.temp_dir(~cleanup=false, prefix, suffix="") =
  dir = file.temp_dir(prefix, suffix)
  if cleanup then on_cleanup({file.rmdir(dir)}) end
  dir
end

# Read the content of a file. Returns a function of type `()->string`. File is
# done reading when function returns the empty string `""`.
# @category File
# @method close Close the underlying file descriptor without waiting for the whole file to be read.
def file.read(fname) =
  fd = file.open(write=false, fname)
  is_done = ref(false)

  def close() =
    if
      not is_done()
    then
      is_done := true
      fd.close()
    end
  end

  def read() =
    if
      is_done()
    then
      ""
    else
      s = fd.read()
      if s == "" then close() end
      s
    end
  end

  read.{close = close}
end

let file.write = ()

# Stream data to a file. Returns a callback to write to the file. Execute
# with `null` or `""` to signify the end of the writing operation. The callback
# can be called from several threads: each call is written as a whole, in the
# order the calls get to run, and the file is closed once.
# @category File
# @method abort Stop without publishing: an atomic write leaves its target alone.
# @param ~append Append data if file exists.
# @param ~perms Default file rights if created. Default: `0o644`.
# @param ~atomic Make the write atomic by writing to a hidden temporary file in \
#                the destination's directory and renaming it over the \
#                destination once writing has succeeded. The destination's \
#                directory must be writable.
# @param p Path to write to.
def file.write.stream(
  ~perms=0o644,
  ~append=false,
  ~atomic:atomic_write=false,
  p
) =
  # Next to the destination, the final rename never crosses file systems.
  temp =
    atomic_write
      ? file.temp(
        directory=path.dirname(p),
        perms=perms,
        ".#{path.basename(p)}.",
        ".tmp"
      )
      : null

  def remove_temp() =
    if null.defined(temp) then file.remove(null.get(temp)) end
  end

  fd =
    try
      file.open(
        write=true,
        append=append and temp == null,
        perms=perms,
        temp ?? p
      )
    catch err do
      remove_temp()
      error.raise(err, err.message)
    end

  finished = ref(false)

  def release() =
    if
      not finished()
    then
      finished := true
      if not fd.closed() then fd.close() end
      remove_temp()
    end
  end

  def fail(err) =
    release()
    error.raise(err, err.message)
  end

  if
    append and null.defined(temp) and file.exists(p)
  then
    try
      read = file.read(p)
      s = ref(read())
      while s() != "" do
        fd.write(s())
        s := read()

      end
    catch err do
      fail(err)
    end
  end

  def finish() =
    finished := true
    try
      fd.close()
      if null.defined(temp) then file.move(atomic=true, null.get(temp), p) end
    catch err do
      finished := false
      fail(err)
    end
  end

  def discard() =
    if
      not finished()
    then
      log(
        label="file.write",
        level=2,
        "Writer for #{p} was dropped before being closed, discarding it."
      )
      release()
    end
  end

  lock = atomic.lock()

  def write(s) =
    s = s ?? ""
    def section() =
      if
        s == ""
      then
        if not finished() then finish() end
      elsif
        finished()
      then
        error.raise(
          error.file,
          "File writer is already closed!"
        )
      else
        try
          fd.write(s)
        catch err do
          fail(err)
        end
      end
    end
    atomic(lock=lock, section)
  end

  def abort() =
    atomic(lock=lock, release)
  end

  runtime.gc.finalise.last(discard, write)
  write.{abort = abort}
end

# Write data to a file.
# @category File
# @param ~data Data to write. If passing a callback `() -> string?`, the callback \
#              must return `null` or `""` when it has finished sending all its data.
# @param ~append Append data if file exists.
# @param ~perms Default file rights if created. Default: `0o644`.
# @param ~atomic Make the write atomic by writing to a hidden temporary file in \
#                the destination's directory and renaming it over the \
#                destination once writing has succeeded. The destination's \
#                directory must be writable. If `data` raises, the \
#                destination is left untouched.
# @param path Path to write to.
def replaces file.write(
  ~data,
  ~perms=0o644,
  ~append=false,
  ~atomic=false,
  path
) =
  cb = file.write.stream(append=append, perms=perms, atomic=atomic, path)

  try
    s = ref(getter.get(data) ?? "")
    cb(s())
    if
      getter.is_constant(data)
    then
      cb("")
    else
      while s() != "" do
        s := getter.get(data) ?? ""
        cb(s())

      end
    end
  catch err do
    cb.abort()
    error.raise(err, err.message)
  end
end

# Ensure that a file exists, creating it empty if it does not.
# @category File
# @param path Path of the file.
def file.touch(~perms=0o644, path) =
  file.write(data="", perms=perms, append=true, path)
end

# Read the whole contents of a file.
# @category File
def file.contents(fname) =
  fn = file.read(fname)

  chunks = ref([])
  next = ref(fn())
  while next() != "" do
    chunks := next()::chunks()
    next := fn()

  end

  string.concat(list.rev(chunks()))
end

# Get the list of lines of a file.
# @category File
def file.lines(fname) =
  r/\n/.split(file.contents(fname))
end

# Iterate over the lines of a file.
# @category File
def file.lines.iterator(fname) =
  list.iterator(file.lines(fname))
end

# Iterate over the contents of a file.
# @category File
def file.iterator(fname) =
  f = file.read(fname)
  fun () ->
    begin
      s = f()
      (s == "") ? null : s
    end
end

# Get a file's mime type by calling the `file` command line binary.
# @category File
def file.mime.cli(fname) =
  mime =
    list.hd(
      default="",
      process.read.lines(
        process.quote.command("file", args=["-b", "--mime-type", fname])
      )
    )

  mime == "" ? null : mime
end

# Get a file's mime type. Uses libmagic if enabled, otherwise try
# to get the value using the file binary. Returns `null` if no value
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
def file.getter(fname) =
  contents = ref("")

  def update() =
    contents := file.contents(fname)
  end

  update()
  watcher = file.watch(fname, update)

  def getter() =
    contents()
  end

  runtime.gc.finalise.last({watcher.unwatch()}, getter)
  getter
end

# Float getter from a file.
# @category File
# @param fname Name of the file from which the contents should be taken.
# @param ~default Default value when the file contains invalid data.
def file.getter.float(~default=0., fname) =
  x = file.getter(fname)

  def f(x) =
    float_of_string(default=default, string.trim(x))
  end

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
    ret =
      string.binary.to_int(
        little_endian=false,
        string.sub(encoding="ascii", s, start=i(), length=4)
      )

    i := i() + 4
    ret
  end

  def read_string(len) =
    ret = string.sub(encoding="ascii", s, start=i(), length=len)
    i := i() + len
    (ret : string)
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
  number_of_colors = number_of_colors > 0 ? null(number_of_colors) : null
  data_len = read_int()
  data = string.sub(encoding="ascii", s, start=i(), length=data_len)
  if
    data == ""
  then
    log.info(
      "Failed to read cover metadata"
    )
    null
  else
    null(
      data.{
        picture_type = pic_type,
        mime = mime,
        description = desc,
        width = width,
        height = height,
        color_depth = color_depth,
        number_of_colors = number_of_colors
      }
    )
  end
end

# Encode cover metadata for embedding with flac files.
# @category String
def file.metadata.flac.cover.encode(
  ~picture_type,
  ~mime,
  ~description="",
  ~width,
  ~height,
  ~color_depth,
  ~number_of_colors=null,
  data
) =
  def encode_string(s) =
    len = 1 + (string.bytes.length(s) / 8)
    str_len = string.binary.of_int(little_endian=false, pad=4, len)
    if
      string.bytes.length(str_len) > 4
    then
      error.raise(
        error.invalid,
        "Data length too long for APIC format!"
      )
    end

    pad = string.make(char_code=0, len * 8 - string.bytes.length(s))
    (str_len, "#{s}#{pad}")
  end

  pic_type = string.binary.of_int(little_endian=false, pad=4, picture_type)
  let (mime_len, mime) = encode_string(mime)
  let (desc_len, description) = encode_string(description)
  width = string.binary.of_int(little_endian=false, pad=4, width)
  height = string.binary.of_int(little_endian=false, pad=4, height)
  color_depth = string.binary.of_int(little_endian=false, pad=4, color_depth)
  number_of_colors =
    string.binary.of_int(little_endian=false, pad=4, number_of_colors ?? 0)

  let (data_len, data) = encode_string(data)
  "#{pic_type}#{mime_len}#{mime}#{desc_len}#{description}#{width}#{height}#{
    color_depth
  }#{number_of_colors}#{data_len}#{data}"
end

# Download file using a regular http.get request. Returns `true` on success.
# @category File
# @param ~filename Downloaded filename.
# @param ~timeout Timeout in seconds
def file.download(~filename, ~timeout=5., url) =
  file_writer = file.write.stream(filename)
  response = http.get.stream(on_body_data=file_writer, timeout=timeout, url)
  response.status_code < 400
end
