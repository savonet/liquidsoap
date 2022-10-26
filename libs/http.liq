# Set of HTTP utils.

%include "http_codes.liq"

# Create a HTTP response string.
# @category Interaction
# @param ~protocol HTTP protocol used.
# @param ~code Response code.
# @param ~headers Response headers.
# @param ~content_type Content type for the data.
# @param ~data Response data.
def http.response(~protocol="HTTP/1.1",
                  ~code=200,~headers=[],
                  ~content_type="",
                  ~data="") =
  status = http.codes[code]
  # Set content-length and connection: close
  headers =
    list.append(headers,
                [("Content-Length", "#{string.length(data)}"),
                 ("Connection", "close")])
  headers =
    if content_type != "" then
      ("Content-Type", content_type)::headers
    else
      headers
    end

  headers = list.map(fun (x) -> "#{fst(x)}: #{snd(x)}",headers)
  headers = string.concat(separator="\r\n",headers)
  # If no headers are provided, we should avoid
  # having an empty line for them. Therefore, we also
  # conditionally add the final \r\n here.
  headers =
    if headers != "" then
      "#{headers}\r\n"
    else
      headers
    end
  resp = ref(
    "#{protocol} #{code} #{status}\r\n\
     #{headers}\
     \r\n\
     #{data}")
  def resp () =
    ret = !resp
    resp := ""
    ret
  end
  resp
end

# Create a streamed HTTP response.
# @category Interaction
# @param ~protocol HTTP protocol used.
# @param ~code Response code.
# @param ~headers Response headers.
# @param ~data_len Response length in bytes.
# @param ~content_type Content type for the data.
# @param ~data Response data.
def http.response.stream(
         ~protocol="HTTP/1.1",
         ~code=200,~headers=[],
         ~content_type="",
         ~data_len,~data) =
  status = http.codes[code]
  # Set content-length and connection: close
  headers =
    list.append(headers,
                [("Content-Length", "#{data_len}"),
                 ("Connection", "close")])
  headers =
    if content_type != "" then
      ("Content-Type", content_type)::headers
    else
      headers
    end

  headers = list.map(fun (x) -> "#{fst(x)}: #{snd(x)}",headers)
  headers = string.concat(separator="\r\n",headers)
  # If no headers are provided, we should avoid
  # having an empty line for them. Therefore, we also
  # conditionally add the final \r\n here.
  headers =
    if headers != "" then
      "#{headers}\r\n"
    else
      headers
    end
  full_head =
    "#{protocol} #{code} #{status}\r\n\
     #{headers}\
     \r\n"
  head_sent = ref(false)
  def resp () =
    if !head_sent then
      data()
    else
      head_sent := true
      full_head
    end
  end
  resp
end

let harbor.http.static = ()

# @flag hidden
def harbor.http.static.base(serve,~content_type,~basepath,~headers,~browse,directory) =
  basepath =
    if string.sub(basepath,start=0,length=1) != "/" then
      "/#{basepath}"
    else
      basepath
    end
  basepath =
    if string.sub(basepath,start=string.length(basepath)-1,length=1) != "/" then
      basepath^"/"
    else
      basepath
    end
  directory = path.home.unrelate(directory)

  responseHeaders = headers

  def handler(~method,~protocol,~data=_,~headers=_,uri)
    ret = regexp("^#{basepath}([^?]*)").exec(uri)
    match =
      if list.length(ret) == 0 then "."
      else string.trim(ret[1]) end
    match = url.decode(match)

    fname = path.concat(directory, match)
    log.debug("Serving static file: #{fname}")
    if file.exists(fname) then
      if file.is_directory(fname) then
        if browse then
          page = ref("")
          def add(s)
            page := !page ^ s ^ "\n"
          end
          def add_file(f)
            add("<li><a href=\"#{uri}/#{url.encode(f)}\">#{f}</a></li>")
          end
          add("<html><body><ul>")
          list.iter(add_file, list.sort(fun (x,y) -> if x<y then -1 elsif x>y then 1 else 0 end, file.ls(fname)))
          add("</ul></body>")
          getter(http.response(protocol=protocol,code=200,headers=responseHeaders,content_type="text/html; charset=UTF-8",data=!page))
        else
          getter(http.response(protocol=protocol,code=403,headers=responseHeaders,content_type="text/plain",data="Forbidden."))
        end
      else
        mime = content_type(fname)
        if method == "OPTIONS" or method == "HEAD" then
          getter(http.response(protocol=protocol,code=200,headers=responseHeaders,content_type=mime))
        else
          getter(http.response.stream(protocol=protocol,code=200,headers=responseHeaders,content_type=mime,data_len=file.size(fname),data=file.read(fname)))
        end
      end
    else
      getter(http.response(protocol=protocol,code=404,headers=responseHeaders,content_type="text/plain",data="Not Found!"))
    end
  end

  def register(method)
    def handler(~protocol,~data,~headers,uri)
      handler(method=method, protocol=protocol, data=data, headers=headers, uri)
    end
    serve(method=method, "^#{basepath}", handler)
  end
  list.iter(register, ["OPTIONS","HEAD","GET"])
end

# It seems that browsers want a trailing 0 for floats.
# @flag hidden
def http.string_of_float(x)
  s = string_of(x)
  n = string.length(s)
  if string.sub(s, start=n-1, length=1) == "." then
    s ^ "0"
  else
    s
  end
end

# @flag hidden
def get_mime_process(file) =
  list.hd(default="", process.read.lines("file -b -I #{process.quote(file)}"))
end

# @flag hidden
content_type = get_mime_process
%ifdef file.mime
# @flag hidden
content_type = file.mime
%endif

# Serve a static path.
# @category Interaction
# @param ~port Port for incoming harbor (http) connections.
# @param ~path Base path.
# @param ~headers Default response headers.
# @param ~browse List files in directories.
# @param ~content_type Callback to specify Content-Type on a per file basis. Default: file.mime if compiled or file CLI if present.
# @param directory Local path to be served.
def replaces harbor.http.static(~port=8000,~path="/",~browse=false,~content_type=content_type,~headers=[("Access-Control-Allow-Origin","*")],directory) =
  # Make the method argument non-optional, see #1018
  serve = fun(~method,uri,handler) -> harbor.http.register(port=port,method=method,uri,handler)
  harbor.http.static.base(serve,content_type=content_type,basepath=path,browse=browse,headers=headers,directory)
end

%ifdef harbor.https.register
# Serve a static path.
# @category Interaction
# @param ~port Port for incoming harbor (https) connections.
# @param ~path Base path.
# @param ~headers Default response headers.
# @param ~browse List files in directories.
# @param ~content_type Callback to specify Content-Type on a per file basis. Default: file.mime if compiled or file CLI if present.
# @param directory Local path to be served.
def harbor.https.static(~port=8000,~path="/",~browse=false,~content_type=content_type,~headers=[("Access-Control-Allow-Origin","*")],directory) =
  serve = fun(~method,uri,handler) -> harbor.https.register(port=port,method=method,uri,handler)
  harbor.http.static.base(serve,basepath=path,browse=browse,content_type=content_type,headers=headers,directory)
end
%endif

# Prepare a list of data to be sent as multipart form data
# @category Interaction
# @param ~boundary Specify boundary to use for multipart/form-data.
# @param data data to insert
def http.multipart_form_data(~boundary=null(), data)
  def default_boundary()
    range = [...string.char.ascii.alphabet, ...string.char.ascii.number]
    l = list.init(12, fun(_) -> string.char.ascii.random(range))
    string.concat(l)
  end
  boundary = null.default(boundary, default_boundary)

  def mk_content(contents, entry) =
    data = entry.contents

    attributes = [("name", entry.name), ...entry.attributes]
    attributes = list.map(fun (v) -> "#{string(fst(v))}=#{string.quote(snd(v))}", attributes)
    attributes = string.concat(separator="; ", attributes)

    headers = list.map(fun (v) -> "#{string(fst(v))}: #{string(snd(v))}", entry.headers)
    headers = string.concat(separator="\r\n", headers)
    headers = headers == "" ? "" : "#{headers}\r\n"

    # This is for typing purposes
    (entry:unit)

    [
      ...contents,
      getter("--#{boundary}\r\n"),
      getter("Content-Disposition: form-data; #{attributes}\r\n"),
      getter(headers),
      getter("\r\n"),
      data,
      getter("\r\n")
    ]
  end

  contents = [
    ...list.fold(mk_content, [], data),
    getter("--#{boundary}--\r\n")
  ]

  contents = string.getter.concat(contents)

  contents =
    if list.for_all(fun (entry) -> getter.is_constant(entry.contents), data) then
      getter(string.getter.flush(contents))
    else
      contents
    end

  { contents = contents, boundary = boundary }
end

# @flag hidden
stdlib_file = file

# @flag hidden
upload_file_fn = fun (~name, ~content_type, ~headers, ~boundary, ~filename, ~file, ~contents, ~timeout, ~redirect, url, fn) -> begin
  if not null.defined(filename) and not null.defined(file) then
    error.raise(error.http, "At least one of: `file` or `filename` must be defined!")
  end

  if null.defined(file) and null.defined(contents) then
    error.raise(error.http, "Only one of: `contents` or `file` must be defined!")
  end

  # Massage parameters
  filename = null.defined(filename) ? null.get(filename) : string(path.basename(null.get(file)))
  contents = null.defined(contents) ? null.get(contents) : getter(stdlib_file.read(null.get(file)))

  # Create query

  content_type = content_type ?? "application/octet-stream"

  data = http.multipart_form_data(boundary=boundary, [{
    name=name,
    attributes=[("filename", filename)],
    headers=[("Content-Type",content_type)],
    contents=contents
  }])
  headers = ("Content-Type", "multipart/form-data; boundary=#{data.boundary}")::headers

  fn(headers=headers, timeout_ms=timeout, redirect=redirect, data=data.contents, url)
end

# Send a file via POST request encoded in multipart/form-data. The contents can
# either be directly specified (with the `contents` argument) or taken from a
# file (with the `file` argument).
# @category Interaction
# @param ~name Name of the field field
# @param ~content_type Content-type (mime) for the file.
# @param ~headers Additional headers.
# @param ~boundary Specify boundary to use for multipart/form-data.
# @param ~filename File name sent in the request.
# @param ~file File whose contents is to be sent in the request.
# @param ~contents Contents of the file sent in the request.
# @param ~timeout Timeout in ms.
# @param ~redirect Follow reidrections.
# @param url URL to post to.
def http.post.file(~name="file", ~content_type=null(), ~headers=[], ~boundary=null(), ~filename=null(), ~file=null(), ~contents=null(), ~timeout=null(), ~redirect=true, url)
  upload_file_fn(name=name, content_type=content_type, headers=headers, boundary=boundary, filename=filename,
                 file=file, contents=contents, timeout=timeout, redirect=redirect, url, http.post)
end

# Send a file via PUT request encoded in multipart/form-data. The contents can
# either be directly specified (with the `contents` argument) or taken from a
# file (with the `file` argument).
# @category Interaction
# @param ~name Name of the field field
# @param ~content_type Content-type (mime) for the file.
# @param ~headers Additional headers.
# @param ~boundary Specify boundary to use for multipart/form-data.
# @param ~filename File name sent in the request.
# @param ~file File whose contents is to be sent in the request.
# @param ~contents Contents of the file sent in the request.
# @param ~timeout Timeout in ms.
# @param ~redirect Follow reidrections.
# @param url URL to put to.
def http.put.file(~name="file", ~content_type=null(), ~headers=[], ~boundary=null(), ~filename=null(), ~file=null(), ~contents=null(), ~timeout=null(), ~redirect=true, url)
  upload_file_fn(name=name, content_type=content_type, headers=headers, boundary=boundary, filename=filename,
                 file=file, contents=contents, timeout=timeout, redirect=redirect, url, http.put)
end
