
# Set of HTTP utils.

%include "http_codes.liq"

# Create a HTTP response string.
# @category Interaction
# @param ~protocol HTTP protocol used.
# @param ~code Response code.
# @param ~headers Response headers.
# @param ~data Response data
def http.response(~protocol="HTTP/1.1",
                  ~code=200,~headers=[],
                  ~data="") = 
  status = http.codes[code]
  # Set content-length and connection: close
  headers = 
    list.append(headers, 
                [("Content-Length", "#{string.length(data)}"),
                 ("Connection", "close")])

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
# @param ~data_len Response length in bytes
# @param ~data Response data
def http.response.stream(
         ~protocol="HTTP/1.1",
         ~code=200,~headers=[],
         ~data_len,~data) =         
  status = http.codes[code]
  # Set content-length and connection: close
  headers =
    list.append(headers,
                [("Content-Length", "#{data_len}"),
                 ("Connection", "close")])

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
def harbor.http.static.base(serve,~content_type,~basepath,~port,~headers,directory) =
  basepath =
    if string.sub(basepath,start=0,length=1) != "/" then
      "/#{basepath}"
    else
      basepath
    end

  responseHeaders = headers

  def handler(~method,~protocol,~data,~headers,uri)
    ret = string.extract(pattern="^#{basepath}([^?]*)",uri)

    if list.length(ret) == 0 then
      http.response(protocol=protocol,code=404,headers=responseHeaders,data="Not Found!")
    else
      match = string.trim(ret[1])

      fname = path.concat(directory, match)
      if file.exists(fname) then
        mime = content_type(fname)

        responseHeaders =
          if mime != "" then
            list.add(("Content-Type",mime),responseHeaders)
          else
            responseHeaders
        end

        if method == "OPTIONS" or method == "HEAD" then
          http.response(protocol=protocol,code=200,headers=responseHeaders)
        else
          http.response.stream(protocol=protocol,code=200,headers=responseHeaders,data_len=file.size(fname),data=file.read(fname))
        end
      else
        http.response(protocol=protocol,code=404,headers=responseHeaders,data="Not Found!")
      end
    end
  end

  def register(method)
    serve(port=port,method=method,"^#{basepath}",handler(method=method))
  end
  list.iter(register, ["OPTIONS","HEAD","GET"])
end

# @flag hidden
def get_mime_process(file) =
  list.hd(default="",get_process_lines("file -b -I #{string.quote(file)}"))
end

# @flag hidden
content_type = get_mime_process
%ifdef file.mime
# @flag hidden
content_type = file.mime
%endif

# Serve a static path
# @category Interaction
# @param ~port Port for incoming harbor (http) connections.
# @param ~path Base path
# @param ~headers Default response headers.
# @param ~content_type Callback to specify Content-Type on a per file basis. Default: file.mime if compiled or file CLI if present.
# @param directory Local path to be served
def replaces harbor.http.static(~port=8000,~path="/",~content_type=content_type,~headers=[("Access-Control-Allow-Origin","*")],directory) =
  harbor.http.static.base(harbor.http.register,content_type=content_type,basepath=path,port=port,headers=headers,directory)
end

%ifdef harbor.https.register
# Serve a static path
# @category Interaction
# @param ~port Port for incoming harbor (https) connections.
# @param ~path Base path
# @param ~headers Default response headers.
# @param ~content_type Callback to specify Content-Type on a per file basis. Default: file.mime if compiled or file CLI if present.
# @param directory Local path to be served
def harbor.https.static(~port=8000,~path="/",~content_type=content_type,~headers=[("Access-Control-Allow-Origin","*")],directory) =
  harbor.http.static.base(harbor.http.register,basepath=path,content_type=content_type,port=port,headers=headers,directory)
end
%endif
