# Set of HTTP utils.

%include "http_codes.liq"

# Create a HTTP response string
# @category Interaction
# @param ~protocol HTTP protocol used.
# @param ~code Response code.
# @param ~headers Response headers.
# @param ~data Response data
def http_response(~protocol="HTTP/1.1",
                  ~code=200,~headers=[],
                  ~data="") = 
  status = http_codes[string_of(code)]
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
  resp = ref
    "#{protocol} #{code} #{status}\r\n\
     #{headers}\
     \r\n\
     #{data}"
  def resp () =
    ret = !resp
    resp := ""
    ret
  end
  resp
end

# Create a streamed HTTP response
# @category Interaction
# @param ~protocol HTTP protocol used.
# @param ~code Response code.
# @param ~headers Response headers.
# @param ~data_len Response length in bytes
# @param ~data Response data
def http_response_stream(
         ~protocol="HTTP/1.1",
         ~code=200,~headers=[],
         ~data_len,~data) =         
  status = http_codes[string_of(code)]
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
  head_sent = ref false
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

# @flag hidden
def harbor.http.static.base(serve,~path,~port,~headers,directory) =
  path =
    if string.sub(path,start=0,length=1) != "/" then
      "/#{path}"
    else
      path
    end

  def handler(~method,~protocol,~data,~headers,uri)
    ret = string.extract(pattern="^#{path}([^?]*)",uri)

    if list.length(ret) == 0 then
      http_response(protocol=protocol,code=404,headers=headers,data="Not Found!")
    else
      match = list.hd(default=("1","foo"),ret)
      file = path.concat(directory, snd(match))
      if file.exists(file) then
        if method == "OPTIONS" then
          http_response(protocol=protocol,code=200,headers=headers)
        else
          http_response_stream(protocol=protocol,code=200,headers=headers,data_len=file.size(file),data=file.read(file))
        end
      else
        http_response(protocol=protocol,code=404,headers=headers,data="Not Found!")
      end
    end
  end

  def register(method)
    serve(port=port,method=method,"^#{path}",handler(method=method))
  end
  list.iter(register, ["OPTIONS","GET"])
end

# Serve a static path
# @category Interaction
# @param ~port Port for incoming harbor (http) connections.
# @param ~path Base path
# @param ~headers Default response headers.
# @param directory Local path to be served
def harbor.http.static(~port=8000,~path="/",~headers=[("Access-Control-Allow-Origin","*")],directory) =
  harbor.http.static.base(harbor.http.register,path=path,port=port,headers=headers,directory)
end

%ifdef harbor.https.register
# Serve a static path
# @category Interaction
# @param ~port Port for incoming harbor (https) connections.
# @param ~path Base path
# @param ~headers Default response headers.
# @param directory Local path to be served
def harbor.https.static(~port=8000,~path="/",~headers=[("Access-Control-Allow-Origin","*")],directory) =
  harbor.http.static.base(harbor.http.register,path=path,port=port,headers=headers,directory)
end
%endif
