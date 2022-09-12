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
# The request also contains the low-level socket associated with the query which the
# caller can use to implement their own custom response, if needed.
# In this case, one should set `custom_response` to `true` on the
# response handler.
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
# The request also contains the low-level socket associated with the query which the
# caller can use to implement their own custom response, if needed.
# In this case, one should set `custom_response` to `true` on the
# response handler.
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
