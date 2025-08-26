path = "/bla"

# BEGIN
def handler(request) =
  log(
    "Got a request on path #{request.path}, protocol version: #{
      request.http_version
    }, method: #{request.method}, headers: #{request.headers}, query: #{
      request.query
    }, body: #{request.body()}"
  )

  http.response(
    content_type="text/html",
    data="<p>ok, this works!</p>"
  )
end

harbor.http.register.simple(port=8080, method="GET", path, handler)
