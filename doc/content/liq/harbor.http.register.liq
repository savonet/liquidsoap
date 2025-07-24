path = "/bla"

# BEGIN
def handler(request, response) =
  log(
    "Got a request on path #{request.path}, protocol version: #{
      request.http_version
    }, method: #{request.method}, headers: #{request.headers}, query: #{
      request.query
    }, body: #{request.body()}"
  )

  # Set response code. Defaults to 200
  response.status_code(201)

  # Set response status message. Uses `status_code` if not specified
  response.status_message("Created")

  # Replaces response headers
  response.headers([("X-Foo", "bar")])

  # Set a single header
  response.header("X-Foo", "bar")

  # Set http protocol version
  response.http_version("1.1")

  # Same as setting the "Content-Type" header
  response.content_type("application/liquidsoap")

  # Set response data. Can be a `string` or a function of type `()->string` returning an empty string
  # when done such as `file.read`
  response.data("foo")

  # Advanced wrappers:

  # Sets content-type to json and data to `json.stringify({foo = "bla"})`
  response.json({foo="bla"})

  # Sets `status_code` and `Location:` header for a HTTP redirect response. Takes an optional `status_code` argument.
  response.redirect("http://...")

  # Sets content-type to html and data to `"<p>It works!</p>"`
  response.html(
    "<p>It works!</p>"
  )
end

harbor.http.register(port=8080, method="GET", path, handler)
