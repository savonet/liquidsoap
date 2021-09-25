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
      list.add(("Content-Type", content_type), headers)
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
      list.add(("Content-Type", content_type), headers)
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
    ret = string.extract(pattern="^#{basepath}([^?]*)", uri)
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
    serve(method=method,"^#{basepath}",handler(method=method))
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
  list.hd(default="", process.read.lines("file -b -I #{string.quote(file)}"))
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

%ifndef http.get
def http.get(~headers=[], ~http_version="http/1.0", ~redirect=true, ~timeout=10, _) =
  ignore(headers)
  ignore(http_version)
  ignore(redirect)
  ignore(timeout)
  error.raise(error.not_found, "Please install ocurl to enable http.get")
  "you should never see this".{headers = [], protocol_version = "", status_code = 999, status_message=""}
end

def http.post(~data="", ~headers=[], ~http_version="http/1.0", ~redirect=true, ~timeout=10, _) =
  ignore(data)
  ignore(headers)
  ignore(http_version)
  ignore(redirect)
  ignore(timeout)
  error.raise(error.not_found, "Please install ocurl to enable http.post")
  "you should never see this".{headers = [], protocol_version = "", status_code = 999, status_message=""}
end

def http.post.stream(~data="", ~headers=[], ~http_version="http/1.0", ~redirect=true, ~timeout=10, _, ~on_body_data=_) =
  ignore(data)
  ignore(headers)
  ignore(http_version)
  ignore(redirect)
  ignore(timeout)
  error.raise(error.not_found, "Please install ocurl to enable http.post.stream")
  ().{headers = [], protocol_version = "", status_code = 999, status_message=""}
end

def http.put(~data="", ~headers=[], ~http_version="http/1.0", ~redirect=true, ~timeout=10, _) =
  ignore(data)
  ignore(headers)
  ignore(http_version)
  ignore(redirect)
  ignore(timeout)
  error.raise(error.not_found, "Please install ocurl to enable http.put")
  "you should never see this".{headers = [], protocol_version = "", status_code = 999, status_message=""}
end

def http.put.stream(~data="", ~headers=[], ~http_version="http/1.0", ~redirect=true, ~timeout=10, _, ~on_body_data=_) =
  ignore(data)
  ignore(headers)
  ignore(http_version)
  ignore(redirect)
  ignore(timeout)
  error.raise(error.not_found, "Please install ocurl to enable http.put.stream")
  ().{headers = [], protocol_version = "", status_code = 999, status_message=""}
end

def http.head(~headers=[], ~http_version="http/1.0", ~redirect=true, ~timeout=10, _) =
  ignore(headers)
  ignore(http_version)
  ignore(redirect)
  ignore(timeout)
  error.raise(error.not_found, "Please install ocurl to enable http.head")
  ().{headers = [], protocol_version = "", status_code = 999, status_message=""}
end

def http.delete(~headers=[], ~http_version="http/1.0", ~redirect=true, ~timeout=10, _) =
  ignore(headers)
  ignore(http_version)
  ignore(redirect)
  ignore(timeout)
  error.raise(error.not_found, "Please install ocurl to enable http.delete")
  ().{headers = [], protocol_version = "", status_code = 999, status_message=""}
end
%endif
