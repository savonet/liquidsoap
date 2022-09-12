Harbor as HTTP server
=====================

The harbor server can be used as a HTTP server. You 
can use the function `harbor.http.register` to register
HTTP handlers. Its parameters are are follow:

```liquidsoap
harbor.http.register(port=8080, method="GET", path, handler)
```
where:

* `port` is the port where to receive incoming connections
* `method` is for the http method (or verb), one of: `"GET"`, `"PUT"`, `"POST"`, `"DELETE"`, `"OPTIONS"` and `"HEAD"`
* `path` is the matched path. It can include named fragments, e.g. `"/users/:id/collabs/:cid"`.
* `handler` is the function used to process requests.

`handler` function is of the form:

```liquidsoap
def handler(request, response) =
  log("Got a request on path #{request.path}, protocol version: #{request.http_version}, \
       method: #{request.method}, headers: #{request.headers}, query: #{request.query}, \
       data: #{request.data}")

  # Sending a response. All values below are optional:
  response.status_code(201) # Defaults to 200
  response.status_message("Created") # Uses `status_code` if not specified
  response.headers(["X-Foo", "bar"])
  response.http_version("1.1")
  response.content_type("application/liquidsoap") # Same as setting the "Content-Type" header
  response.data("foo") # Can also be function of type `()->string` returning an empty string
                       # when done such as `file.read`
end
```

The handler function receives a record containing all the information about the request and fills
up the details about the response, which is then used to write a proper HTTP response to the client.

Named fragments from the request path are passed to the response `query` list.

Advanced users also have access to the request's socket, which makes it possible to implement your own interaction 
using the socket's `read`, `write` and `close` methods. In this case, you should set `response.custom(true)` on the
response.

For advanced path matching, the function `harbor.http.register.regexp` can be used. It behaves exactly like `harbor.http.register`
except that it accepts regular expressions to be used to match the request path. Named capture groups in the regular
expression are also passed via the request `query` parameter.

These functions can be used to create your own HTTP interface. Some examples
are:

Redirect Icecast's pages
------------------------
Some source clients using the harbor may also request pages that
are served by an icecast server, for instance listeners statistics.
In this case, you can register the following handler:

```liquidsoap
# Redirect all files other
# than /admin.* to icecast,
# located at localhost:8000
def redirect_icecast(request, response) =
  response.status_code(301)
  response.headers([("Location","http://localhost:8000#{request.path}")])
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

Get metadata
------------
You can use harbor to register HTTP services to 
fecth/set the metadata of a source. For instance, 
using the [JSON export function](json.html) `json.stringify`:

```liquidsoap
meta = ref([])

# s = some source
s.on_metadata(fun (m) -> meta := m)

# Return the json content of meta
def get_meta(_, response) =
  response.content_type("application/json; charset=utf-8")
  response.data(json.stringify(!meta))
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

Set metadata
------------
Using `insert_metadata`, you can register a GET handler that
updates the metadata of a given source. For instance:

```liquidsoap
# s = some source

# Create a source equiped with a `insert_metadata` method:
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

  response.content_type("text/html")
  response.data("<html><body><b>#{ret}</b></body></html>")
end

# Register handler on port 700
harbor.http.register(port=7000,method="GET","/setmeta",set_meta)
```

Now, a request of the form `http://server:7000/setmeta?title=foo`
will update the metadata of source `s` with `[("title","foo")]`. You
can use this handler, for instance, in a custom HTML form.
