# Set of HTTP utils.

# Prepare a list of data to be sent as multipart form data.
# @category Internet
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

# Initiate a response handler with pre-filled values.
# @category Internet
# @method content_type Set `"Content-Type"` header
# @method data Set response data.
# @method headers Replace response headers.
# @method header Set a single header on the response
# @method json Set content-type to json and data to `json.stringify` of the argument
# @method redirect Set `status_code` and `Location:` header for a HTTP redirect response
# @method html Set content-type to html and data to argument value
# @method http_version Set http protocol version
# @method status_code Set response status code
# @method status_message Set response status message
def http.response(~http_version="1.1",
                  ~status_code=null(),
                  ~status_message=null(),
                  ~headers=[],
                  ~content_type=null(),
                  ~data=getter("")) =
  status_code = status_code ??
    if http_version == "1.1" and headers["expect"] == "100-continue" and getter.get(data) == "" then
      100
    else
      200
    end

  http_version = ref(http_version)
  status_code = ref(status_code)
  status_message = ref(status_message)
  headers = ref(headers)
  content_type = ref(content_type)
  data = ref(data)
  status_sent = ref(false)
  headers_sent = ref(false)
  data_sent = ref(false)
  response_ended = ref(false)

  def mk_status() =
    status_sent := true

    http_version = http_version()
    status_code = status_code()

    status_code =
      if status_code == 100 and getter.get(data()) != "" then
        200
      else
        status_code
      end

    status_message = status_message() ?? http.codes[status_code]

    "HTTP/#{http_version} #{status_code} #{status_message}\r\n"
  end

  def mk_headers() =
    headers_sent := true

    headers = headers()
    content_type = content_type()
    data = data()

    headers =
      if getter.is_constant(data) then
        data = getter.get(data)
        if data != "" then
          ("Content-Length", "#{string.length(data)}")::headers
        else
          headers
        end
      else
        ("Transfer-Encoding", "chunked")::headers
      end

    headers =
      if null.defined(content_type) and null.get(content_type) != "" then
        ("Content-type", null.get(content_type))::headers
      else
        headers
      end

    headers = list.map(fun (v) -> "#{fst(v)}: #{snd(v)}", headers)
    headers = string.concat(separator="\r\n", headers)
    headers = if headers != "" then "#{headers}\r\n" else "" end

    "#{headers}\r\n"
  end

  def mk_data() =
    data_sent := true

    data = data()

    if getter.is_constant(data) then
      response_ended := true
      getter.get(data)
    else
      data = getter.get(data)
      response_ended := data == ""
      "#{string.hex_of_int(string.length(data))}\r\n#{data}\r\n"
    end
  end

  def response() =
    if response_ended() then
      ""
    elsif not status_sent() then
      mk_status()
    elsif not headers_sent() then
      mk_headers()
    else
      mk_data()
    end
  end

  def attr_method(sent, attr) =
    def set(v) =
      if sent() then
        error.raise(error.invalid, "HTTP response has already been sent for this value!")
      end
      attr := v
    end

    def get() = attr() end

    set.{current=get}
  end

  def header(k, v) =
    headers := (k, v)::headers()
  end

  code = status_code
  def redirect(~status_code=301, location) =
    if status_sent() then
      error.raise(error.invalid, "HTTP response has already been sent for this value!")
    end
    code := status_code
    header("Location", location)
  end

  def json(~compact=true, v) =
    if headers_sent() then
      error.raise(error.invalid, "HTTP response has already been sent for this value!")
    end
    content_type := "application/json; charset=utf-8"
    data := json.stringify(v, compact=compact) ^ "\n"
  end

  def html(d) =
    if headers_sent() then
      error.raise(error.invalid, "HTTP response has already been sent for this value!")
    end
    content_type := "text/html"
    data := d
  end

  def send_status(socket) =
    if not status_sent() then
      socket.write(mk_status())
    end
  end

  def multipart_form(~boundary=null(), contents) =
    if headers_sent() then
      error.raise(error.invalid, "HTTP response has already been sent for this value!")
    end

    form_data = http.multipart_form_data(boundary=boundary, contents)

    content_type := "multipart/form-data; boundary=#{form_data.boundary}"
    data := form_data.contents
  end

  response.{
    http_version = attr_method(status_sent, http_version),
    status_code = attr_method(status_sent, status_code),
    status_message = attr_method(status_sent, status_message),
    headers = attr_method(headers_sent, headers),
    header = header,
    redirect = redirect,
    json = json,
    html = html,
    content_type = attr_method(headers_sent, content_type),
    multipart_form = multipart_form,
    data = attr_method(data_sent, data),
    send_status = send_status,
    status_sent = {status_sent()}
  }
end

# @flag hidden
def harbor.http.regexp_of_path(path) =
  def named_capture(s) =
    name = string.sub(s, start=1, length=string.length(s)-1)
    "(?<#{name}>[^/]+)"
  end
  rex = r/:[\w_]+/g.replace(named_capture, path)
  regexp("^#{rex}$")
end

# @flag hidden
def harbor.http.mk_body(get_data) =
  done = ref(false)
  data = ref("")
  def body(~timeout=10.) =
    if done() then data() else
      start_time = time()
      def rec read() =
        if done() then data() else
          if start_time+timeout < time() then
            error.raise(error.http, "Timeout!")
          end

          r = get_data(timeout=timeout)
          if r == "" then data() else
            data := "#{data()}#{r}"
            read()
          end
        end
      end
      read()
    end
  end

  body
end

# Register a HTTP handler on the harbor. This function offers a simple API,
# suitable for quick implementation of HTTP handlers. See `harbor.http.register`
# for a node/express like alternative API.
# @category Internet
# @argsof harbor.http.register
def harbor.http.register.simple(%argsof(harbor.http.register), path, handler) =
  def handler(request) =
    def data(~timeout=10.) =
      request.data(timeout=timeout)
    end

    handler(request.{
      data = data,
      body = harbor.http.mk_body(data)
    })
  end

  harbor.http.register(%argsof(harbor.http.register), harbor.http.regexp_of_path(path), handler)
end

# Register a HTTP handler on the harbor with a generic regexp `path`. This function offers a simple API,
# suitable for quick implementation of HTTP handlers. See `harbor.http.register`
# for a node/express like alternative API.
# @category Internet
# @argsof harbor.http.register
def harbor.http.register.simple.regexp(%argsof(harbor.http.register), path, handler) =
  harbor.http.register(%argsof(harbor.http.register), path, handler)
end

# @flag hidden
let harbor.http.middleware = ref(fun (req, res, next) -> next(req, res))

# Register a new harbor middleware
# @category Internet
def harbor.http.middleware.register(fn) =
  middleware = harbor.http.middleware()
  harbor.http.middleware := fun (req, res, next) -> begin
    middleware(req, res, fun (res, res) ->
      fn(req, res, next))
  end
end

# @flag hidden
def harbor.http.register.regexp(%argsof(harbor.http.register), path, handler) =
  def handler(request) =
    response = http.response(
      http_version=request.http_version,
    )

    is_response_done = ref(false)

    def replaces response() =
      ret = response()
      is_response_done := ret == ""
      ret
    end

    def data(~timeout=10.) =
      if is_response_done() then
        error.raise(error.http, "Response ended!")
      end

      if response.status_code.current() == 100 and not response.status_sent() then
        response.send_status(request.socket)
      end
      request.data(timeout=timeout)
    end

    request = {
      body = harbor.http.mk_body(data),
      data = data,
      headers = request.headers,
      http_version = request.http_version,
      method = request.method,
      path = request.path,
      query = request.query
    }

    handler = fun (req, res) -> begin
      middleware = harbor.http.middleware()
      middleware(req, res, fun (req, res) -> handler(req, res))
    end

    (handler(request, response):unit)

    response
  end

  harbor.http.register(%argsof(harbor.http.register), path, handler)
end

def replaces harbor.http.middleware = () end

# Register a HTTP handler on the harbor. The handler function
# receives as argument the full requested information and returns the
# answer sent to the client, including HTTP headers. This function
# registers exact path matches, i.e. `"/users"`, `"/index.hml"`
# as well as fragment matches, i.e. `"/user/:id"`, `"/users/:id/collabs/:cid"`,
# etc. If you need more advanced matching, use `harbor.http.register.regexp`
# to match regular expressions. Paths are resolved in the order they are declared
# and can override default harbor paths such as metadata handlers.
# The handler receives the request details as a record and a response
# handler. Matched fragments are reported as part of the response `query` parameter.
# The response handler can be used to fill up details about the http response,
# which will be converted into a plain HTTP response string after the handler returns.
# @category Internet
# @argsof harbor.http.register
def replaces harbor.http.register(%argsof(harbor.http.register), path, handler) =
  harbor.http.register.regexp(%argsof(harbor.http.register), harbor.http.regexp_of_path(path), handler)
end

let harbor.http.static = ()

# @flag hidden
def harbor.http.static.base(serve,~content_type,~basepath,~headers,~browse,directory) =
  directory = path.home.unrelate(directory)
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

  def handler(request, response)
    response.headers(headers)

    rpath = string.residual(prefix=basepath, request.path)
    if not null.defined(rpath) then
      response.status_code(404)
    else
      rpath = null.get(rpath)
      fname = path.concat(directory, rpath)
      log.debug("Serving static file: #{fname}")
      if not file.exists(fname) then
        response.status_code(404)
      else
        if file.is_directory(fname) then
          if not browse then
            response.status_code(403)
          else
            page = ref("")
            def add(s)
              page := page() ^ s ^ "\n"
            end
            def add_file(f)
              add("<li><a href=\"#{request.path}#{url.encode(f)}\">#{f}</a></li>")
            end
            add("<html><body><ul>")
            list.iter(add_file, file.ls(sorted=true, fname))
            add("</ul></body>")
            response.content_type("text/html; charset=UTF-8")
            response.data(string.getter.single(page()))
          end
        else
          mime = content_type(fname)

          if null.defined(mime) then
            response.content_type(null.get(mime))
          end

          if request.method == "GET" then
            response.data(file.read(fname))
          end
        end
      end
    end
  end

  basepath = "#{basepath}.*"
  def register(method)
    serve(method=method, basepath, handler)
  end
  list.iter(register, ["OPTIONS","HEAD","GET"])
end

# It seems that browsers want a trailing 0 for floats.
# @flag hidden
def http.string_of_float(x)
  s = string(x)
  n = string.length(s)
  if string.sub(s, start=n-1, length=1) == "." then
    s ^ "0"
  else
    s
  end
end

# @flag hidden
def get_mime_process(file) =
  mime = list.hd(default="", process.read.lines("file -b -I #{process.quote(file)}"))
  if mime == "" then null() else mime end
end

# @flag hidden
content_type = get_mime_process
%ifdef file.mime
# @flag hidden
content_type = file.mime
%endif

# Serve a static path.
# @category Internet
# @param ~port Port for incoming harbor (http) connections.
# @param ~transport Http transport. Use `http.transport.ssl` or http.transport.secure_transport`, when available, to enable HTTPS output
# @param ~path Base path.
# @param ~headers Default response headers.
# @param ~browse List files in directories.
# @param ~content_type Callback to specify Content-Type on a per file basis. Default: file.mime if compiled or file CLI if present.
# @param directory Local path to be served.
def replaces harbor.http.static(~transport=http.transport.unix,~port=8000,~path="/",~browse=false,~content_type=(content_type:(string)->string?),~headers=[("Access-Control-Allow-Origin","*")],directory) =
  # Make the method argument non-optional, see #1018
  serve = fun(~method,uri,handler) -> harbor.http.register(transport=transport,port=port,method=method,uri,handler)
  harbor.http.static.base(serve,content_type=content_type,basepath=path,browse=browse,headers=headers,directory)
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

  fn(headers=headers, timeout=timeout, redirect=redirect, data=data.contents, url)
end

# Send a file via POST request encoded in multipart/form-data. The contents can
# either be directly specified (with the `contents` argument) or taken from a
# file (with the `file` argument).
# @category Internet
# @param ~name Name of the field field
# @param ~content_type Content-type (mime) for the file.
# @param ~headers Additional headers.
# @param ~boundary Specify boundary to use for multipart/form-data.
# @param ~filename File name sent in the request.
# @param ~file File whose contents is to be sent in the request.
# @param ~contents Contents of the file sent in the request.
# @param ~timeout Timeout in seconds.
# @param ~redirect Follow reidrections.
# @param url URL to post to.
def http.post.file(~name="file", ~content_type=null(), ~headers=[], ~boundary=null(), ~filename=null(), ~file=null(), ~contents=null(), ~timeout=null(), ~redirect=true, url)
  upload_file_fn(name=name, content_type=content_type, headers=headers, boundary=boundary, filename=filename,
                 file=file, contents=contents, timeout=timeout, redirect=redirect, url, http.post)
end

# Send a file via PUT request encoded in multipart/form-data. The contents can
# either be directly specified (with the `contents` argument) or taken from a
# file (with the `file` argument).
# @category Internet
# @param ~name Name of the field field
# @param ~content_type Content-type (mime) for the file.
# @param ~headers Additional headers.
# @param ~boundary Specify boundary to use for multipart/form-data.
# @param ~filename File name sent in the request.
# @param ~file File whose contents is to be sent in the request.
# @param ~contents Contents of the file sent in the request.
# @param ~timeout Timeout in seconds.
# @param ~redirect Follow reidrections.
# @param url URL to put to.
def http.put.file(~name="file", ~content_type=null(), ~headers=[], ~boundary=null(), ~filename=null(), ~file=null(), ~contents=null(), ~timeout=null(), ~redirect=true, url)
  upload_file_fn(name=name, content_type=content_type, headers=headers, boundary=boundary, filename=filename,
                 file=file, contents=contents, timeout=timeout, redirect=redirect, url, http.put)
end

let harbor.http.request = ()

let settings.http = settings.make.void(
  "Settings for HTTP requests"
)
let settings.http.mime = settings.make.void(
  "MIME-related settings for HTTP requests"
)

let settings.http.mime.extnames = settings.make(
  description="MIME to file extension mappings", [
  ("audio/mp3", ".mp3"),
  ("audio/mpeg", ".mp3"),
  ("application/ogg", ".ogg"),
  ("application/x-ogg", ".ogg"),
  ("audio/x-ogg", ".ogg"),
  ("audio/ogg", ".ogg"),
  ("video/ogg", ".ogg"),
  ("audio/flac", ".flac"),
  ("audio/x-flac", ".flac"),
  ("audio/mp4", ".mp4"),
  ("application/mp4", ".mp4"),
  ("video/mp4", ".mp4"),
  ("audio/vnd.wave", ".wav"),
  ("audio/wav", ".wav"),
  ("audio/wave", ".wav"),
  ("audio/x-wav", ".wav"),
  ("image/jpeg", ".jpg"),
  ("image/png", ".png"),
  ("application/pdf", ".pdf"),
  ("image/bmp", ".bmp")
])

let http.headers = ()

# Extract the content-type header
# @category Internet
def http.headers.content_type(headers) =
  mime =
    try
      list.find(fun (v) -> begin
        let (header_name, _) = v
        string.case(lower=true, header_name) == "content-type"
      end, headers)
    catch _ : [error.not_found] do
      null()
    end
  mime = null.map(snd, mime)

  null.map(fun (mime) -> begin
    let [mime, ...args] = list.map(string.trim, string.split(separator=";", mime))

    def parse_arg(arg) =
      let [name, ...value] = string.split(separator="=", arg)
      (name, string.unquote(string.concat(separator="=", value)))
    end

    {mime=mime, args=list.map(parse_arg, args)}
  end, mime)
end

# Extract the content-disposition header
# @category Internet
def http.headers.content_disposition(headers) =
  content_disposition =
    try
      list.find(fun (v) -> begin
        let (header_name, _) = v
        string.case(lower=true, header_name) == "content-disposition"
      end, headers)
    catch _ : [error.not_found] do
      null()
    end

  def parse_arg(arg) =
    let [name, ...value] = string.split(separator="=", arg)
    (name, string.unquote(string.concat(separator="=", value)))
  end

  def parse_filename(args) =
    plain_filename = args["filename"]
    plain_filename = plain_filename == "" ? null() : plain_filename

    encoded_filename = args["filename*"]
    encoded_filename = encoded_filename == "" ? null() : encoded_filename
    encoded_filename = null.map(fun (encoded_filename) -> begin
      let [encoding, _, filename] = string.split(separator="'", encoded_filename)
      string.recode(in_enc=encoding, filename)
    end, encoded_filename)

    filename = null.defined(encoded_filename) ? encoded_filename : plain_filename
    filename = null.map(fun (filename) -> url.decode(string.unquote(filename)), filename)

    (filename, list.filter(fun (v) -> fst(v) != "filename" and fst(v) != "filename*", args))
  end

  def parse_name(args) =
    name = args["name"]
    name = name == "" ? null() : name
    name = null.map(fun (name) -> url.decode(string.unquote(name)), name)
    (name, list.filter(fun (v) -> fst(v) != "name", args))
  end

  null.map(fun (v) -> begin
    let (_, header_value) = v
    let [type, ...args] = list.map(string.trim, string.split(separator=";", header_value))
    args = list.map(parse_arg, args)
    let (filename, args) = parse_filename(args)
    let (name, args) = parse_name(args)
    ({type = type, filename = filename, name = name, args = args}:{ type: string, filename?: string, name?: string, args: [(string*string?)]})
  end, content_disposition)
end

# Try to get a filename from a request's headers.
# @category Internet
def http.headers.extname(headers) =
  content_disposition = http.headers.content_disposition(headers)
  content_type = http.headers.content_type(headers)

  extname =
    if null.defined(content_disposition?.filename) then
      extname = file.extension(null.get(content_disposition?.filename))
      extname == "" ? null() : extname
    else
      null()
    end

  if null.defined(extname) then
    extname
  elsif null.defined(content_type) then
    extnames = settings.http.mime.extnames()
    extname = extnames[null.get(content_type).mime]
    extname == "" ? null() : extname
  else
    null()
  end
end
