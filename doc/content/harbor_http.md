Harbor as HTTP server
=====================
The harbor server can be used as a HTTP server. You
can use the function `harbor.http.register` to register
HTTP handlers. Its parameters are are follow:

```liquidsoap
harbor.http.register(port=8080,method="GET",uri,handler)
```
where:

* `port` is the port where to receive incoming connections
* `method` is for the http method (or verb), one of: `"GET"`, `"PUT"`, `"POST"`, `"DELETE"`, `"OPTIONS"` and `"HEAD"`
* `uri` is used to match requested uri. Perl regular expressions are accepted.

* `handler` is the function used to process requests.

`handler` function has type:

```
(~protocol:string, ~data:string,
 ~headers:[(string*string)], string)->'a))->unit
where 'a is either string or ()->string
```

where:

* `protocol` is the HTTP protocol used by the client. Currently, one of `"HTTP/1.0"` or `"HTTP/1.1"`
* `data` is the data passed during a POST request
* `headers` is the list of HTTP headers sent by the client
* `string` is the (unparsed) uri requested by the client, e.g.: `"/foo?var=bar"`

The `handler` function returns HTTP and HTML data to be sent to the client,
for instance:

```
HTTP/1.1 200 OK\r\n\
Content-type: text/html\r\n\
Content-Length: 35\r\n\
\r\n\
<html><body>It works!</body></html>
```

(`\r\n` should always be used for line return
in HTTP content)

The handler is a _string getter_, which means that it can be of either type `string` or type `()->string`.
The former is used to return the response in one call while the later can be used to returned bigger response
without having to load the whole response string in memory, for instance in the case of a file.

For convenience, two functions, `http.response` and `http.response.stream` are provided to
create a HTTP response string. `http.response` has the following type:

```
(?protocol:string,?code:int,?headers:[(string*string)],
 ?data:string)->string
```

where:

* `protocol` is the HTTP protocol of the response (default `HTTP/1.1`)
* `code` is the response code (default `200`)
* `headers` is the response headers. It defaults to `[]` but an appropriate `"Content-Length"` header is added if not set by the user and `data` is not empty.
* `data` is an optional response data (default `""`)

`http.response.stream` has the following type:

```
(?protocol:string,?code:int,?headers:[(string*string)],
 data_len:int,data:()->string)->string
```

where:

* `protocol` is the HTTP protocol of the response (default `HTTP/1.1`)
* `code` is the response code (default `200`)
* `headers` is the response headers. It defaults to `[]` but an appropriate `"Content-Length"` header is added if not set by the user and `data` is not empty.
* `data_len` is the length of the streamed response
* `data` is the response stream

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
def redirect_icecast(~protocol,~data,~headers,uri) =
   http.response(
     protocol=protocol,
     code=301,
     headers=[("Location","http://localhost:8000#{uri}")]
   )
end

# Register this handler at port 8005
# (provided harbor sources are also served
#  from this port).
harbor.http.register(port=8005,method="GET",
                     "^/(?!admin)",
                     redirect_icecast)
```

Another alternative, less recommended, is to
directly fetch the page's content from the Icecast server:

```liquidsoap
# Serve all files other
# than /admin.* by fetching data
# from Icecast, located at localhost:8000
def proxy_icecast(~protocol,~data,~headers,uri) =
  def f(x) =
    # Replace Host
    if string.case(lower=false, fst(x)) == "HOST" then
      "Host: localhost:8000"
    else
      "#{fst(x)}: #{snd(x)}"
    end
  end
  headers = list.map(f,headers)
  headers = string.concat(separator="\r\n",headers)
  request =
    "#{method} #{uri} #{protocol}\r\n\
     #{headers}\r\n\r\n"
  process.read("echo #{quote(request)} | \
                      nc localhost 8000")
end

# Register this handler at port 8005
# (provided harbor sources are also served
#  from this port).
harbor.http.register(port=8005,method="GET",
                     "^/(?!admin)",
                     proxy_icecast)
```

This method is not recommended because some servers may not
close the socket after serving a request, causing `nc` and
liquidsoap to hang.

Get metadata
------------
You can use harbor to register HTTP services to
fecth/set the metadata of a source. For instance,
using the [JSON export function](json.html) `json.stringify`:

```liquidsoap
meta = ref([])

# s = some source

# Update current metadata
# converted in UTF8
def update_meta(m) =
  m = metadata.export(m)
  recode = string.recode(out_enc="UTF-8")
  def f(x) =
    (recode(fst(x)),recode(snd(x)))
  end
  meta := list.map(f,m)
end

# Apply update_metadata
# every time we see a new
# metadata
s = on_metadata(update_meta,s)

# Return the json content
# of meta
def get_meta(~protocol,~data,~headers,uri) =
  m = !meta
  http.response(
    protocol=protocol,
    code=200,
    headers=[("Content-Type","application/json; charset=utf-8")],
    data=json.stringify(m)
  )
end

# Register get_meta at port 700
harbor.http.register(port=7000,method="GET","/getmeta",get_meta)
```

Once the script is running,
a GET/POST request for `/getmeta` at port `7000`
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

Which can be used with AJAX-based backends to fetch the current
metadata of source `s`

Set metadata
------------
Using `insert_metadata`, you can register a GET handler that
updates the metadata of a given source. For instance:

```liquidsoap
# s = some source

# Create a source equiped with a `insert_metadata` method:
s = insert_metadata(s)

# The handler
def set_meta(~protocol,~data,~headers,uri) =
  # Split uri of the form request?foo=bar&...
  # into (request,[("foo","bar"),..])
  x = url.split(uri)

  # Filter out unusual metadata
  meta = metadata.export(snd(x))

  # Grab the returned message
  ret =
    if meta != [] then
      s.insert_metadata(meta)
      "OK!"
    else
      "No metadata to add!"
  end

  # Return response
  http.response(
   protocol=protocol,
   code=200,
   headers=[("Content-Type","text/html")],
   data="<html><body><b>#{ret}</b></body></html>"
  )
end

# Register handler on port 700
harbor.http.register(port=7000,method="GET","/setmeta",set_meta)
```

Now, a request of the form `http://server:7000/setmeta?title=foo`
will update the metadata of source `s` with `[("title","foo")]`. You
can use this handler, for instance, in a custom HTML form.

Limitations
===========
When using harbor's HTTP server, please be warned that the server is
**not** meant to be used under heavy load. Therefore, it should **not**
be exposed to your users/listeners if you expect many of them. In this
case, you should use it as a backend/middle-end and have some kind of
caching between harbor and the final user. In particular, the harbor server
is not meant to server big files because it loads their entire content in
memory before sending them. However, the harbor HTTP server is fully equipped
to serve any kind of CGI script.
