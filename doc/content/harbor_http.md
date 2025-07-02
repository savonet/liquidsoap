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

For convenience, a HTTP response builder is provided via `http.response`. Here's an example:

```{.liquidsoap include="harbor.http.response.liq" from="BEGIN"}

```

where:

- `port` is the port where to receive incoming connections
- `method` is for the http method (or verb), one of: `"GET"`, `"PUT"`, `"POST"`, `"DELETE"`, `"OPTIONS"` and `"HEAD"`
- `path` is the matched path. It can include named fragments, e.g. `"/users/:id/collabs/:cid"`. Named named fragments are passed via `request.query`, for instance: `req.query["cid"]`.

## Node/express API

The `harbor.http.register` function offers a higher-level API for advanced HTTP response implementation.
Its API is very similar to the node/express API. Here's an example:

```{.liquidsoap include="harbor.http.register.liq" from="BEGIN"}

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

`https` is supported using either `libssl` or `ocaml-tls`. When compiled with either of them, a `http.transport.ssl` or `http.transport.tls`
is available and can be passed to each `harbor` operator:

```liquidsoap
transport = http.transport.ssl(
  # Server mode: required,
  # client mode: optional, add certificate to trusted pool
  certificate="/path/to/certificate/file",

  # Server mode: required, client mode: ignored
  key="/path/to/secret/key/file",

  # Required if key file requires one.
  # TLS does not support password encrypted keys!
  password="optional password"
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

```{.liquidsoap include="harbor-simple.liq"}

```

## Examples

These functions can be used to create your own HTTP interface. Some examples
are:

## Redirect Icecast's pages

Some source clients using the harbor may also request pages that
are served by an icecast server, for instance listeners statistics.
In this case, you can register the following handler:

```{.liquidsoap include="harbor-redirect.liq"}

```

## Get metadata

You can use harbor to register HTTP services to
fecth/set the metadata of a source.

```{.liquidsoap include="harbor-metadata.liq" from="BEGIN"}

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

Using source's `insert_metadata` method, you can register a GET handler that
updates the metadata of a given source. For instance:

```{.liquidsoap include="harbor-insert-metadata.liq" from="BEGIN"}

```

Now, a request of the form `http://server:7000/setmeta?title=foo`
will update the metadata of source `s` with `[("title","foo")]`. You
can use this handler, for instance, in a custom HTML form.
