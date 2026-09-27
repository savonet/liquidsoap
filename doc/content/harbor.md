# Harbor input and output

This page covers the Icecast-style streaming operators built on the harbor
server. With `input.harbor`, source clients push their stream to Liquidsoap,
the way they push it to an Icecast server. With `output.harbor`, listeners
connect to Liquidsoap to play your stream, the way they connect to an Icecast
server. To pull a stream from a remote server, see
[pulling a remote stream](#pulling-a-remote-stream). To use the harbor server
as a general web server, with your own HTTP endpoints, see
[harbor as HTTP server](./harbor_http.md).

Liquidsoap can receive live streams from source clients using the Icecast or
Shoutcast (ICY) source protocol, via the `input.harbor` and
`input.harbor.dynamic` operators. When one of these is used, Liquidsoap opens a
network port and waits for an incoming connection. Once a client connects and
starts sending audio or video data, the source becomes available and can be used
in your script like any other source.

This is the standard way to add live inputs to a Liquidsoap stream: you point
your encoder (e.g. Butt, Mixxx, Liquidsoap itself) at the harbor port, and your
script switches to and from the live source using `fallback` or similar
operators.

`input.harbor` is a live source, so `fallback([live, music])` already does the
right thing without any parameters: the DJ cuts in mid-song, the song is faded
out underneath, and the music starts a fresh track when they disconnect. See
[source composition](./composition.md) for how that is decided and how to change
it.

Two variants are available:

- `input.harbor` is static: it registers one mountpoint with one expected content type at startup.
- `input.harbor.dynamic` is dynamic: it uses FFmpeg to detect the stream format at connection time and calls your callback with the stream information.

## `input.harbor`

### Basic usage

`input.harbor` listens on a fixed mountpoint and port. The source is fallible:
it is available when a client is connected and unavailable otherwise.

```{.liquidsoap include="harbor-usage.liq" to=-1}

```

The unlabeled argument is the mountpoint. Source clients connect to
`http://<host>:<port>/<mountpoint>`. For Shoutcast clients, use `"/"` as the
mountpoint.

### Authentication

By default, connections are authenticated with a fixed `user` and `password`.
For more control, use the `auth` parameter to provide a custom function. It
receives a record with `user`, `password`, `address`, `uri`, `query` and
`method` fields and must return a boolean:

```{.liquidsoap include="harbor-auth.liq"}

```

For ICY (Shoutcast) connections, there is no username in the source protocol.
The `user` parameter value is used instead, and passed to the `auth` function.

### Global settings

Global harbor settings can be listed with `liquidsoap --list-settings`. The
most relevant ones are:

- `settings.harbor.bind_addrs`: List of IP addresses the server listens on. Defaults to `["0.0.0.0"]` (all interfaces). Set it to `["127.0.0.1"]` to accept local connections only.
- `settings.harbor.timeout`: Timeout for network operations, in seconds. Defaults to `120.`. Each `input.harbor` also has its own `timeout` parameter, `30.` by default.
- `settings.harbor.verbose`: Log passwords used by source clients. Useful for debugging. Defaults to `false`.
- `settings.harbor.reverse_dns`: Resolve client IP addresses to hostnames. Defaults to `false`.
- `settings.harbor.icy_formats`: MIME types for which ICY metadata updates are allowed. Defaults to common audio formats.

### Per-source settings

Key parameters for `input.harbor`:

- `port`: Port to listen on. Defaults to `8005`. Different inputs can use different ports; mountpoints are scoped per port.
- `user`, `password`: Credentials for source connections.
- `auth`: Custom authentication function (see above).
- `icy`: Enable ICY (Shoutcast) source protocol. Defaults to `false`.

When ICY is enabled on port `n`, harbor also listens on port `n+1`, where Shoutcast source clients send their data, following the Shoutcast convention.

## `input.harbor.dynamic`

`input.harbor.dynamic` is an advanced operator for building systems that react
to incoming stream connections. Mountpoints are matched with a regexp or with
`:id` placeholders, the stream format is detected at connection time using
FFmpeg, and your callback receives the stream information and decides what to
do with the stream.

Use it to route streams by URI or to apply different processing to each
stream. Each new connection is handled on its own, and the callback sees the
stream information before any audio or video is processed. Liquidsoap's
[icecast server emulation](./icecast_server.md) is built on
`input.harbor.dynamic`.

Supported container formats include MP3, OGG, FLAC, AAC, MKV, WebM, MP4, FLV,
MPEG-TS, and anything else FFmpeg can demux.

### The `on_connect` callback

The callback has the signature `(connection_record) -> source -> unit`. It is
first called with a connection record describing the incoming stream, and must
return a function that will be called with the live source. You can inspect the
stream in the first call and return a handler function suited to its content
type, based on `streams`.

The connection record contains:

| Field          | Type                  | Description                                           |
| -------------- | --------------------- | ----------------------------------------------------- |
| `uri`          | `string`              | The request URI                                       |
| `query`        | `[(string * string)]` | Named capture groups from a regexp mountpoint         |
| `format`       | `string?`             | Detected container format, e.g. `"ogg"`, `"matroska"` |
| `streams`      | `[stream_info]`       | List of detected streams (see below)                  |
| `headers`      | `[(string * string)]` | HTTP headers from the connecting client               |
| `copy_encoder` | `(?string) -> format` | Pre-built encoder for passthrough muxing              |

Each entry in `streams` is a record with:

- `field`, `type`, `codec`: always present. `type` is `"audio"`, `"video"`, `"subtitle"` or `"data"`.
- `samplerate`, `channels`, `channel_layout`: present for audio streams.
- `width`, `height`, `pixel_format`, `frame_rate`: present for video streams.

To refuse a connection, raise an error in the callback (in either the first or
second call).

### `copy_encoder`

The `copy_encoder` field is the simplest way to route an incoming stream: it
produces an encoder that remuxes the stream as is, keeping the original
quality. Call it with no argument to keep the original
container format, or pass a format string to override it:

```liquidsoap
c.copy_encoder()           # keep original container
c.copy_encoder("ogg")     # remux into OGG
```

### Examples

**Basic relay: record each incoming stream to a file**

```{.liquidsoap include="harbor-dynamic-basic.liq"}

```

**URI-based routing using `:name` placeholders**

```{.liquidsoap include="harbor-dynamic-routing.liq"}

```

The plain `input.harbor.dynamic` accepts a path string where `:word` segments
are placeholders that match any path component. Each placeholder is available
by name in `c.query`.

**URI-based routing using a full regexp**

For more control, `input.harbor.dynamic.regexp` accepts a Liquidsoap regexp
directly. Named capture groups (`(?<name>...)`) are available by name in
`c.query`, which allows matching more specific patterns:

```{.liquidsoap include="harbor-dynamic-regexp.liq"}

```

**Filtering: reject connections without an audio stream**

```{.liquidsoap include="harbor-dynamic-filter.liq"}

```

### Known limitations

- The `streams` field describes what FFmpeg detected. Your pipeline has to
  match it: using the wrong encoder for the stream produces a runtime error.

- Type inference between the callback's source and the FFmpeg decoder works in
  most cases but may fail in some advanced scenarios, such as remuxing using
  the `streams` parameter directly.

For now, `copy_encoder` is the most reliable approach and covers the majority
of use cases.

## `output.harbor`

`output.harbor` turns Liquidsoap into an HTTP streaming server for listeners,
serving the encoded stream over HTTP in a way compatible with Icecast/Shoutcast
clients.

### Authentication

Authentication is configured via the `auth` function. It receives a record with
`address`, `login`, and `password` fields and must return a boolean:

```liquidsoap
output.harbor(
  mount="/stream",
  auth=fun({address, login, password}) ->
    login == "source" and password == "secret",
  %mp3,
  s
)
```

When `auth` is `null` (the default), all connections are accepted without
authentication.

### Dedicated encoder mode

By default, `output.harbor` uses a shared encoder: a single encoder instance
is started with the output and its data is sent to all connected listeners.
Each listener receives the codec header and any buffered burst data on
connection.

When `dedicated_encoder=true`, a new encoder is created for each listener at
connection time, so every listener starts from a fresh encoder state:

```liquidsoap
output.harbor(
  mount="/stream",
  dedicated_encoder=true,
  %ffmpeg(format="mp3", %audio.copy),
  s
)
```

Dedicated encoder mode is useful for copy-only formats such as `%ffmpeg` in
copy mode, where starting mid-stream can cause decoding issues on the client
side. For encoded formats (e.g. `%mp3`, `%aac`), it adds one full encoder
instance per connected listener, which can use a lot of CPU with many
listeners. The `burst` parameter only applies to the shared encoder.

### Listener callbacks

`on_connect` is called when a listener connects, before it receives any data, and `on_disconnect` when it disconnects. Both receive a record describing the listener:

| Field          | Type                  | Description                                                                           |
| -------------- | --------------------- | ------------------------------------------------------------------------------------- |
| `id`           | `int`                 | Connection identifier, unique for the output                                          |
| `ip`           | `string`              | Client address, without port                                                          |
| `uri`          | `string`              | Requested URI                                                                         |
| `protocol`     | `string`              | HTTP protocol version (e.g. `"1.1"`)                                                  |
| `headers`      | `[(string * string)]` | HTTP headers from the client                                                          |
| `connected_at` | `float`               | Connection time, in seconds since the epoch                                           |
| `duration`     | `float`               | Seconds connected: `0.` in `on_connect`, the session length in `on_disconnect`        |
| `bytes_sent`   | `int`                 | Bytes sent, HTTP response included: `0` in `on_connect`, the total in `on_disconnect` |

Synchronous `on_disconnect` handlers are called exactly once for each listener, after its synchronous `on_connect` handlers, so they can be used to keep per-listener state. Asynchronous handlers each run in their own thread, in any order.

The `listeners` method returns the currently connected listeners, as the same records with `duration` and `bytes_sent` so far.

```liquidsoap
o = output.harbor(mount="/stream", %mp3, s)
o.on_connect(synchronous=true, fun (l) -> log("#{l.ip} connected"))
o.on_disconnect(
  synchronous=true,
  fun (l) -> log("#{l.ip} disconnected after #{l.duration}s")
)
```

## SSL / HTTPS

The harbor server is shared by `input.harbor`, `output.harbor` and the
[HTTP server](./harbor_http.md) functions, so HTTPS is set up the same way for
all of them. SSL support requires the `ssl` opam package, which provides
`http.transport.ssl`, or the `tls` opam package, which provides
`http.transport.tls`. Every harbor operator takes a `transport` argument; pass
it one of these transports:

```liquidsoap
transport =
  http.transport.ssl(
    certificate="/path/to/cert.pem", key="/path/to/key.pem"
  )
s = input.harbor(transport=transport, port=8005, "live")
```

The `key` argument can be omitted when the certificate file also contains the
private key. If the key file is protected by a password, pass it with the
`password` argument of `http.transport.ssl`. Only `http.transport.ssl`
supports password-protected keys. The same transport is accepted by
`output.harbor` and `harbor.http.register`.

A port uses one transport at a time. Registering handlers, sources or outputs
on the same port with different transports raises an `error.http` error.

Client operators such as `output.icecast` also take a `transport` argument. In
client mode, the `certificate` argument is optional: when you pass it, the
certificate is added to the list of trusted certificates.

### Renewing certificates

Certificates expire: a Let's Encrypt certificate lasts 90 days and is renewed
on disk well before that. The transport reads the certificate and key when the
first port using it opens, and then again once a day, when the first client of
the day connects. A renewed certificate is therefore picked up within a day,
with nothing to set up. Clients already connected keep the certificate they
started with, new ones get the renewed one.

The `reload_on` argument replaces that daily check. It is looked at on every
new connection, and the files are read again whenever it returns `true`. If
they cannot be loaded, an error is logged and the previous certificate stays
in use.

To apply a renewal right away, call the transport's `reload()` method, for
instance from a server command run by a certbot deploy hook:

```{.liquidsoap include="harbor-tls-reload.liq"}

```

`reload()` raises an error when the files cannot be loaded, and the previous
certificate stays in use. `certificate` and `key` also accept getters, so a
reload can switch to different paths:

```{.liquidsoap include="harbor-tls-getter.liq"}

```

The same applies to `http.transport.tls`, whose `client_certificate` argument is
read again on reload as well.

For a free, valid certificate, see [Let's Encrypt](https://letsencrypt.org/).
For local testing, a self-signed certificate can be generated with:

```
openssl req -x509 -newkey rsa:4096 -sha256 -nodes \
  -keyout server.key -out server.crt \
  -subj "/CN=localhost" -days 3650
```

## Pulling a remote stream

The harbor operators wait for a client to connect and push a stream to
Liquidsoap. You can also do the reverse: Liquidsoap pulls its data from a
remote location. This location can be a distant file or playlist, or an
icecast or shoutcast stream.

To use it in your script, simply create a source that way:

```{.liquidsoap include="http-input.liq" from="BEGIN" to="END"}

```

`input.http` is based on `input.ffmpeg`. On top of it, `input.http` reads the
ICY metadata sent by icecast and shoutcast servers, and sets `self_sync`
automatically when it detects such a server. `input.ffmpeg` accepts any URL
that FFmpeg can open, for instance an HLS playlist or an SRT stream. Both
operators take these parameters:

- `poll_delay`: Delay between two connection attempts when the stream is unavailable. Defaults to `2.`.
- `max_buffer`: Maximum duration of buffered data, in seconds. Defaults to `5.`.
- `format`: Force a specific input format. The format is autodetected when this is `null`, the default.
- `self_sync`: Let the source control its own timing. Defaults to `false` for `input.ffmpeg`. For `input.http`, the default `null` enables it when the remote server is an icecast or shoutcast server.

`input.http` also takes a `timeout` for the connection, `10.` by default, and a
`user_agent`. Connection events are available with the source's
`on_connect` and `on_disconnect` methods.

This operator will regularly poll the given location for its data, so it should
be used for locations that are assumed to be available most of the time. If
not, it might generate unnecessary traffic and pollute the logs. In this case,
it is perhaps better to invert the paradigm and use the
[input.harbor](#inputharbor) operator.
