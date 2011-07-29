# Set of HTTP utils.

%include "http_codes.liq"

# Create a HTTP response string
# @param ~protocol HTTP protocol used.
# @param ~code Response code.
# @param ~headers Response headers.
# @param ~data Response data
def http_response(~protocol="HTTP/1.1",
                  ~code=200,
                  ~headers=[],
                  ~data="") = 
  status = http_codes[string_of(code)]
  # Set content-length if needed and not set by the
  # user.
  headers = 
    if data != "" and 
       not list.mem_assoc("Content-Length",headers) 
    then
      list.append([("Content-Length",
                    "#{string.length(data)}")],
                  headers)
    else
      headers
    end
  headers = list.map(fun (x) -> "#{fst(x)}: #{snd(x)}",headers)
  headers = string.concat(separator="\r\n",headers)
  "#{protocol} #{code} #{status}\r\n\
   #{headers}\r\n\
   \r\n\
   #{data}"
end
