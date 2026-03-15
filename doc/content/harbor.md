# Harbor input

Liquidsoap can receive live streams from source clients using the Icecast or
Shoutcast (ICY) source protocol, via the `input.harbor` and
`input.harbor.dynamic` operators. When one of these is used, Liquidsoap opens a
network port and waits for an incoming connection. Once a client connects and
starts sending audio or video data, the source becomes available and can be used
in your script like any other source.

This is the standard way to integrate live inputs into a Liquidsoap stream: you
point your encoder (e.g. Butt, Mixxx, Liquidsoap itself) at the harbor port,
and your script handles the transition to and from the live source using
`fallback` or similar operators.

Two variants are available:

- **`input.harbor`** — static: one mountpoint, one expected content type, registered at startup.
- **`input.harbor.dynamic`** — dynamic: uses FFmpeg to detect the stream format at connection time and calls a user-defined callback with full stream information.

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
receives a record with `user`, `password`, and `address` fields and must return
a boolean:

```{.liquidsoap include="harbor-auth.liq"}

```

For ICY (Shoutcast) connections, there is no username in the source protocol.
The `user` parameter value is used instead, and passed to the `auth` function.

### Global settings

Global harbor settings can be listed with `liquidsoap --list-settings`. The
most relevant ones are:

- `harbor.bind_addrs`: List of IP addresses the server listens on. Defaults to `["0.0.0.0"]` (all interfaces). Restrict to `["127.0.0.1"]` to accept local connections only.
- `harbor.timeout`: Timeout for source connections, in seconds. Defaults to `30.`.
- `harbor.verbose`: Log passwords used by source clients. Useful for debugging. Defaults to `false`.
- `harbor.reverse_dns`: Resolve client IP addresses to hostnames. Defaults to `true`.
- `harbor.icy_formats`: MIME types for which ICY metadata updates are allowed. Defaults to common audio formats.

### Per-source settings

Key parameters for `input.harbor`:

- `port`: Port to listen on. Defaults to `8005`. Different inputs can use different ports; mountpoints are scoped per port.
- `user`, `password`: Credentials for source connections.
- `auth`: Custom authentication function (see above).
- `icy`: Enable ICY (Shoutcast) source protocol. Defaults to `false`.

When ICY is enabled on port `n`, Shoutcast clients should connect to port `n+1`.

### SSL / HTTPS

SSL support requires one of the following opam packages: `ssl` or
`osx-secure-transport`. When available via `ssl`, use `input.harbor.ssl`;
when available via `osx-secure-transport`, use `input.harbor.secure_transport`.

The corresponding settings are under `harbor.ssl.*` or
`harbor.secure_transport.*`:

- `harbor.ssl.certificate`: Path to the SSL certificate.
- `harbor.ssl.private_key`: Path to the SSL private key.
- `harbor.ssl.password`: Optional password to unlock the private key.

For a free, valid certificate, see [Let's Encrypt](https://letsencrypt.org/).
For local testing, a self-signed certificate can be generated with:

```
openssl req -x509 -newkey rsa:4096 -sha256 -nodes \
  -keyout server.key -out server.crt \
  -subj "/CN=localhost" -days 3650
```

## `input.harbor.dynamic`

`input.harbor.dynamic` is an advanced operator for building systems that react
dynamically to incoming stream connections. Unlike `input.harbor`, everything
is dynamic: mountpoints are matched via regexp or `:id` placeholders, the
stream format is detected at connection time using FFmpeg, and a user-defined
callback receives full stream information and decides what to do with it.

This makes it the right tool for building sophisticated ingest systems —
routing streams by URI, applying per-stream logic, or serving as the foundation
of a full Icecast server clone in Liquidsoap. The callback-based design means
each new connection can be handled independently, with full access to stream
metadata before any audio or video is processed.

Supported container formats include MP3, OGG, FLAC, AAC, MKV, WebM, MP4, FLV,
MPEG-TS, and anything else FFmpeg can demux.

### The `on_connect` callback

The callback has the signature `(connection_record) -> source -> unit`. It is
first called with a connection record describing the incoming stream, and must
return a function that will be called with the live source. This two-step design
lets you inspect the stream before deciding how to handle it, and allows you to
select a pre-determined handler function for each content type based on
`streams` without having to dispatch inside a single catch-all function.

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

- `field`, `type`, `codec` — always present
- `samplerate`, `channels`, `channel_layout` — present for audio streams
- `width`, `height`, `pixel_format`, `frame_rate` — present for video streams

To refuse a connection, raise an error in the callback (in either the first or
second call).

### `copy_encoder`

The `copy_encoder` field is the most straightforward way to route an incoming
stream: it produces an encoder that remuxes the stream without re-encoding,
preserving the original quality. Call it with no argument to keep the original
container format, or pass a format string to override it:

```liquidsoap
c.copy_encoder()           # keep original container
c.copy_encoder("ogg")     # remux into OGG
```

### Examples

**Basic relay — record each incoming stream to a file:**

```{.liquidsoap include="harbor-dynamic-basic.liq"}

```

**URI-based routing using `:name` placeholders:**

```{.liquidsoap include="harbor-dynamic-routing.liq"}

```

The plain `input.harbor.dynamic` accepts a path string where `:word` segments
are placeholders that match any path component. Each placeholder is available
by name in `c.query`.

**URI-based routing using a full regexp:**

For more control, `input.harbor.dynamic.regexp` accepts a Liquidsoap regexp
directly. Named capture groups (`(?<name>...)`) are available by name in
`c.query`, which allows matching more specific patterns:

```{.liquidsoap include="harbor-dynamic-regexp.liq"}

```

**Filtering — reject connections without an audio stream:**

```{.liquidsoap include="harbor-dynamic-filter.liq"}

```

### Known limitations

- **Stream info is descriptive only.** The `streams` field tells you what FFmpeg
  detected, but Liquidsoap does not enforce that your pipeline matches it. Using
  the wrong encoder for the stream will produce a runtime error.

- **Type inference edge cases.** Type inference between the callback's source
  and the FFmpeg decoder works in most cases but may fail in some advanced
  scenarios, such as remuxing using the `streams` parameter directly. These
  cases will be addressed in future releases.

For now, `copy_encoder` is the most reliable approach and covers the majority
of use cases.
