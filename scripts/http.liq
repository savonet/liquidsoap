# Set of HTTP utils.

%include "http_codes.liq"

# Create a HTTP response string
# @category Interaction
# @param ~protocol HTTP protocol used.
# @param ~code Response code.
# @param ~headers Response headers.
# @param ~data Response data
def http_response(~protocol="HTTP/1.1",
                  ~code=200,
                  ~headers=[],
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
  "#{protocol} #{code} #{status}\r\n\
   #{headers}\
   \r\n\
   #{data}"
end
