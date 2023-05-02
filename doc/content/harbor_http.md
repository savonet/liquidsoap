# Harbor as HTTP server

The harbor server can be used as a HTTP server. We provide two type of APIs for this:

## Simple API

The `harbor.http.register.simple` function provides a simple, easy to use registration API for quick
HTTP response implementation. This function receives a record describing the request and returns
the HTTP response.

The request passed to the function contains all expected information from the underlying HTTP
query.

The `data` method on a request is a _string getter_, that is a function of type: `() -> string`
which returns the empty string `""` when all data has been consumed. You can use this function
to e.g. write the request data to a file using `file.write.stream`.

The `body` method can be used to read all of the request's data and store it in
memory. Make sure to only use it if you know that the response should be small enough!

For convenience, a HTTP response builder is provided via `harbor.http.response`. Here's an example:

```liquidsoap
def handler(request) =
  log("Got a request on path #{request.path}, protocol version: #{request.http_version}, \
       method: #{request.method}, headers: #{request.headers}, query: #{request.query}, \
       body: #{request.body()}")

  harbor.http.response(
    content_type="text/html",
    data="<p>ok, this works!</p>"
  )
end

harbor.http.register.simple(port=8080, method="GET", path, handler)
```

where:

- `port` is the port where to receive incoming connections
- `method` is for the http method (or verb), one of: `"GET"`, `"PUT"`, `"POST"`, `"DELETE"`, `"OPTIONS"` and `"HEAD"`
- `path` is the matched path. It can include named fragments, e.g. `"/users/:id/collabs/:cid"`. Named named fragments are passed via `request.query`, for instance: `req.query["cid"]`.

## Node/express API

The `harbor.http.register` function offers a higher-level API for advanced HTTP response implementation.
Its API is very similar to the node/express API. Here's an example:

```liquidsoap
def handler(request, response) =
  log("Got a request on path #{request.path}, protocol version: #{request.http_version}, \
       method: #{request.method}, headers: #{request.headers}, query: #{request.query}, \
       body: #{request.body()}")

  # Set response code. Defaults to 200
  response.status_code(201)

  # Set response status message. Uses `status_code` if not specified
  response.status_message("Created")

  # Replaces response headers
  response.headers(["X-Foo", "bar"])

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
  response.json({foo = "bla"})

  # Sets `status_code` and `Location:` header for a HTTP redirect response. Takes an optional `status_code` argument.
  response.redirect("http://...")

  # Sets content-type to html and data to `"<p>It works!</p>"`
  response.html("<p>It works!</p>")
end

harbor.http.register(port=8080, method="GET", path, handler)
```

where:

- `port` is the port where to receive incoming connections
- `method` is for the http method (or verb), one of: `"GET"`, `"PUT"`, `"POST"`, `"DELETE"`, `"OPTIONS"` and `"HEAD"`
- `path` is the matched path. It can include named fragments, e.g. `"/users/:id/collabs/:cid"`. Matched named fragments are passed via `request.query`, for instance: `req.query["cid"]`.

The handler function receives a record containing all the information about the request and fills
up the details about the response, which is then used to write a proper HTTP response to the client.

Named fragments from the request path are passed to the response `query` list.

Middleware _a la_ node/express are also supported and registered via `http.harbor.middleware.register`. See `http.harbor.middleware.cors` for an example of how to implement one such middleware.

Here's how you would enable the `cors` middleware:

```
harbor.http.middleware.register(harbor.http.middleware.cors(origin="example.com"))
```

## Https support

`https` is supported using either `libssl` or macos' `SecureTransport`. When compiled with either of them, a `http.transport.ssl` or `http.transport.secure_transport`
is available and can be passed to each `harbor` operator:

```liquidsoap
transport = http.transport.ssl(
  certificate="/path/to/certificate/file", # Server mode: required, client mode: optional, add certificate to trusted pool
  key="/path/to/secret/key/file", # Server mode: required, client mode: ignored
  password="optional password" # Required if key file requires one
)

harbor.http.register(transport=transport, port=8000, ...)

input.harbor(transport=..., port=8000, ...)

output.harbor(transport=..., port=8000, ...)

output.icecast(transport=..., port=8000, ...)
```

A given port can only support one type of transport at a time and registering handlers, sources or outputs on the same port with different transports
will raise a `error.http` error.

## Advanced usage

All registration functions have a `.regexp` counter part, e.g. `harbor.http.register.simple.regexp`. These function accept
a full regular expression for their `path` argument. Named matches on the regular expression are also passed via the request's `query`
parameter.

It is also possible to directly interact with the underlying socket using the `simple` API:

```liquidsoap
  # Custom response
  def handler(req) =
    req.socket.write("HTTP/1.0 201 YYR\r\nFoo: bar\r\n\r\n")
    req.socket.close()

    # Null indicates that we're using the socket directly.
    null()
  end

  harbor.http.register.simple("/custom", port=3456, handler)
```

## Examples

These functions can be used to create your own HTTP interface. Some examples
are:

## Redirect Icecast's pages

Some source clients using the harbor may also request pages that
are served by an icecast server, for instance listeners statistics.
In this case, you can register the following handler:

```liquidsoap
# Redirect all files other
# than /admin.* to icecast,
# located at localhost:8000
def redirect_icecast(request, response) =
  response.redirect("http://localhost:8000#{request.path}")
end

# Register this handler at port 8005
# (provided harbor sources are also served
#  from this port).
harbor.http.register.regexp(
  port=8005,
  method="GET",
  r/^\/(?!admin)/,
  redirect_icecast
)
```

## Get metadata

You can use harbor to register HTTP services to
fecth/set the metadata of a source.

```liquidsoap
meta = ref([])

# s = some source
s.on_metadata(fun (m) -> meta := m)

# Return the json content of meta
def get_meta(_, response) =
  response.json(!meta)
end

# Register get_meta at port 700
harbor.http.register(port=7000,method="GET","/getmeta",get_meta)
```

Once the script is running,
a GET request for `/getmeta` at port `7000`
returns the following:

```
HTTP/1.1 200 OK
Content-Type: application/json; charset=utf-8

{
  "genre": "Soul",
  "album": "The Complete Stax-Volt Singles: 1959-1968 (Disc 8)",
  "artist": "Astors",
  "title": "Daddy Didn't Tell Me"
}
```

## Set metadata

Using `insert_metadata`, you can register a GET handler that
updates the metadata of a given source. For instance:

```liquidsoap
# s = some source

# Create a source equipped with a `insert_metadata` method:
s = insert_metadata(s)

# The handler
def set_meta(request, response) =
  # Filter out unusual metadata
  meta = metadata.export(request.query)

  # Grab the returned message
  ret =
    if meta != [] then
      s.insert_metadata(meta)
      "OK!"
    else
      "No metadata to add!"
  end

  response.html("<html><body><b>#{ret}</b></body></html>")
end

# Register handler on port 700
harbor.http.register(port=7000,method="GET","/setmeta",set_meta)
```

Now, a request of the form `http://server:7000/setmeta?title=foo`
will update the metadata of source `s` with `[("title","foo")]`. You
can use this handler, for instance, in a custom HTML form.
