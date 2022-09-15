# Set of HTTP utils.

%include "http_codes.liq"

# @flag hidden
def harbor.http.response(~http_version="1.1",
                         ~status_code=200,
                         ~status_message=null(),
                         ~headers=[],
                         ~content_type=null(),
                         ~data=getter("")) = 
  http_version = ref(http_version)
  status_code = ref(status_code)
  status_message = ref(status_message)
  headers = ref(headers)
  content_type = ref(content_type)
  data = ref(data)
  headers_sent = ref(false)
  response_ended = ref(false)

  def mk_headers() =
    headers_sent := true

    http_version = !http_version
    status_code = !status_code
    status_message = !status_message
    headers = !headers
    content_type = !content_type
    data = !data

    status_message = status_message ?? http.codes[status_code]

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

    "HTTP/#{http_version} #{status_code} #{status_message}\r\n\
     #{headers}\r\n"
  end

  def mk_data() =
    data = !data

    if getter.is_constant(data) then
      response_ended := true
      getter.get(data)
    else
      data = getter.get(data)
      response_ended := data == ""
      "#{string.length(data)}\r\n#{data}\r\n"
    end
  end

  def response() =
    if !response_ended then
      ""
    else
      if not !headers_sent then
        mk_headers()
      else
        mk_data()
      end
    end
  end

  def attr_method(attr) =
    def set(v) =
      if !headers_sent then
        error.raise(error.invalid, "HTTP response headers already sent!")
      end
      attr := v
    end

    def get() = !attr end

    set.{current=get}
  end

  response.{
    http_version = attr_method(http_version),
    status_code = attr_method(status_code),
    status_message = attr_method(status_message),
    headers = attr_method(headers),
    header = (fun (l, v) -> headers := (l, v)::!headers),
    content_type = attr_method(content_type),
    data = attr_method(data)
  } 
end

# Register a HTTP handler on the harbor. This is the low-level function
# that is exported from liquidsoap's core. Use this if you want to implement
# a custom HTTP response. Otherwise, you are advised to use one of the higher-level
# wrappers `harbor.http.register` or `harbor.http.register.regexp`
# @category Liquidsoap
# @argsof harbor.http.register
def harbor.http.register.core(%argsof(harbor.http.register), path, handler) = 
  harbor.http.register(%argsof(harbor.http.register), path, handler)
end

def replaces harbor.http.register(%argsof(harbor.http.register), path, handler) =
  def handler(request) =
    request = {
      data = request.data,
      headers = request.headers,
      http_version = request.http_version,
      method = request.method,
      path = request.path,
      query = request.query
    }

    response = harbor.http.response(
      http_version=request.http_version,
    )

    handler(request, response)

    response
  end
    
  harbor.http.register.core(%argsof(harbor.http.register), path, handler)
end

# @flag hidden
def harbor.http.regexp_of_path(path) =
  def named_capture(s) =
    name = string.sub(s, start=1, length=string.length(s)-1)
    "(?<#{name}>[^/]+)"
  end
  regexp(r/:[\w_]+/g.replace(named_capture, path))
end

let harbor.http.register.regexp = harbor.http.register

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
# @category Liquidsoap
# @argsof harbor.http.register
def replaces harbor.http.register(%argsof(harbor.http.register), path, handler) =
  harbor.http.register.regexp(%argsof(harbor.http.register), harbor.http.regexp_of_path(path), handler)
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

  def handler(request, response)
    response.headers(headers)

    fname = path.concat(directory, request.path)
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
            page := !page ^ s ^ "\n"
          end
          def add_file(f)
            add("<li><a href=\"#{request.path}/#{url.encode(f)}\">#{f}</a></li>")
          end
          add("<html><body><ul>")
          list.iter(add_file, file.ls(sorted=true, fname))
          add("</ul></body>")
          response.content_type("text/html; charset=UTF-8")
          response.data({!page})
        end
      else
        mime = content_type(fname)

        if mime != "" then
          response.content_type(mime)
        end

        if request.method == "GET" then
          response.data(file.read(fname))
        end
      end
    end
  end

  def register(method)
    serve(method=method, basepath, handler)
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
# Register a HTTPS handler on the harbor. This is the low-level function
# that is exported from liquidsoap's core. Use this if you want to implement
# a custom HTTPS response. Otherwise, you are advised to use one of the higher-level
# wrappers `harbor.http.register` or `harbor.http.register.regexp`
# @category Liquidsoap
# @argsof harbor.https.register
let harbor.https.register.core = harbor.https.register

def replaces harbor.https.register(%argsof(harbor.https.register), path, handler) =
  def handler(request) =
    response = harbor.http.response(
      http_version=request.http_version,
    )

    handler(request, response)

    response
  end

  harbor.https.register.core(%argsof(harbor.https.register), path, handler)
end

let harbor.https.register.regexp = harbor.https.register

# Register a HTTPS handler on the harbor. The handler function
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
# @category Liquidsoap
# @argsof harbor.https.register
def replaces harbor.https.register(%argsof(harbor.https.register), path, handler) =
  harbor.https.register.regexp(%argsof(harbor.https.register), harbor.http.regexp_of_path(path), handler)
end

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
