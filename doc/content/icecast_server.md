# Icecast Server Emulation

Liquidsoap can act as an icecast-compatible server, accepting source client connections using the `icecast.server` operator. This allows you to receive streams from software like butt, mixxx, or any other icecast-compatible source client.

## Experimental Feature

This functionality is still experimental. While it works for many common use cases, some features may change in future releases and some icecast configuration options are not yet supported.

## Basic Usage

The simplest way to start an icecast-compatible server:

```{.liquidsoap include="icecast-server-basic.liq"}

```

This starts a server on port 8000 with the default password "hackme".

### Parameters

- `port`: Port to listen on (default: 8000)
- `password`: Source password for authentication (default: "hackme")

Parameters passed explicitly take precedence over the values of a configuration file.

- `config`: Optional path to an icecast XML configuration file
- `dedicated_encoder`: Allocate one encoder per listener (see [below](#dedicated_encoder))
- `serve`: Enable or disable the built-in status page (default: `true`, see [below](#status-page))
- `serve_auth`: Optional callback `(request) -> bool` for access control on status endpoints
- `serve_json`: Optional callback `(stats) -> string` to replace the built-in JSON renderer
- `serve_html`: Optional callback `(stats) -> string` to replace the built-in HTML renderer
- `x_forwarded_for_proxy_ips`: List of known reverse-proxy IPs for real-IP extraction (see [below](#reverse-proxy-and-x-forwarded-for))
- `x_forwarded_for`: Advanced callback to fully override real-IP extraction logic (see [below](#reverse-proxy-and-x-forwarded-for))
- `format_options`: Optional callback `(string) -> [(string * string)]` that returns muxer options for a given container format name. When `null`, falls back to `settings.icecast.server.default_muxer_options` (see [below](#live-streaming-muxer-options))
- `ip_hash`: Function applied to every listener IP before it is exposed anywhere (see [below](#listener-privacy))
- `access_log`: Path of an icecast-style access log, `-` for standard error (see [below](#access-and-playlist-logs))
- `playlist_log`: Path of an icecast-style playlist log, `-` for standard error
- `admin_user`: User name for the admin listener page (default: `"admin"`)
- `admin_password`: Password for the admin listener page, which is disabled without one (see [below](#admin-listener-page))

### Return Value

The `icecast.server` function returns a record with the following methods:

- `mounts()`: Returns a list of currently active mount points
- `get_source(mount)`: Returns the source for a given mount point
- `get_config(mount)`: Returns the current `{format, streams}` record for a mount, or `null` if the mount is not active
- `stats()`: Returns the current mount stats list (see [below](#custom-json-renderer) for its fields)
- `on_source_connect(handler)`: Register a handler called when a source connects
- `on_source_disconnect(handler)`: Register a handler called when a source disconnects
- `on_listener_connect(handler)`: Register a handler called when a listener connects (see [below](#listener-callbacks))
- `on_listener_disconnect(handler)`: Register a handler called when a listener disconnects
- `on_metadata(handler)`: Register a handler called when a mount's metadata changes

Every `on_*` method takes an optional `synchronous` argument, `false` by default, in which case the handler runs in its own thread.

## Listener Callbacks

Knowing who listens, for how long, and what they were listening to is what most station tooling is built on: listener statistics, royalty reports, dashboards or a Prometheus exporter. `icecast.server` reports every listener session through callbacks, so you can feed that data wherever you need it:

```{.liquidsoap include="icecast-server-listener-callbacks.liq"}

```

Listener handlers receive a record with the following fields:

| Field          | Type      | Description                                                                     |
| -------------- | --------- | ------------------------------------------------------------------------------- |
| `id`           | `int`     | Connection identifier, unique per mount output                                  |
| `mount`        | `string`  | Mount the listener is connected to                                              |
| `ip`           | `string`  | Listener address after X-Forwarded-For resolution, hashed by default            |
| `user_agent`   | `string?` | `User-Agent` request header                                                     |
| `referer`      | `string?` | `Referer` request header                                                        |
| `uri`          | `string`  | Requested path                                                                  |
| `protocol`     | `string`  | HTTP protocol version, e.g. `"1.1"`                                             |
| `connected_at` | `float`   | Connection time, in seconds since the epoch                                     |
| `duration`     | `float`   | Seconds connected: `0.` on connect, the session length on disconnect            |
| `bytes_sent`   | `int`     | Bytes sent, HTTP response headers included: `0` on connect, total on disconnect |

Raw request headers are not passed on: they can carry the listener's address, for instance in `X-Forwarded-For`.

Synchronous disconnect handlers are called exactly once per listener, and always after its synchronous connect handlers. Asynchronous handlers each run in their own thread, so that order is not guaranteed for them.

`on_metadata` handlers receive a record with the `mount`, its new `metadata` and the current number of `listeners`.

## Access and Playlist Logs

Many log analyzers and statistics tools read the access log written by icecast. `icecast.server` can write the same log, so these tools keep working when liquidsoap takes over serving listeners:

```{.liquidsoap include="icecast-server-access-log.liq"}

```

When an icecast configuration file is used, the logs follow its `<logging>` section instead, see the [configuration reference](#logging).

### Access log

One line is written when a listener disconnects, in icecast's variant of the combined log format, with the session length in seconds appended:

```
8c2f0e7a1d4b9f36 - - [14/Sep/2026:21:40:58 +0200] "GET /live HTTP/1.1" 200 1843200 "-" "VLC/3.0.20 LibVLC/3.0.20" 115
```

The fields are the listener IP (hashed by default), identity and user (always `-`), disconnection time, request, status code, bytes sent including the HTTP response headers, referer, user agent and duration. As in icecast, control characters, spaces, `!`, `"`, `` ` `` and `\` are written as `\xHH` escapes, except that spaces are kept in the referer and user agent. Bytes above `0x7f` are written as-is, so UTF-8 text stays readable.

Some tools validate the first field as an IP address and reject hashed values. For instance, [GoAccess](https://goaccess.io/) needs `--no-ip-validation`. You can also [log plain IPs](#listener-privacy).

Unlike icecast, only listener sessions are logged: source client connections and requests to the status pages are not.

### Playlist log

One line is written each time a mount's metadata changes:

```
14/Sep/2026:21:40:58 +0200|/live|12|Artist - Title
```

The fields are the time, mount, number of listeners and the `artist - title` text. Icecast writes the text as-is. Liquidsoap turns `|` characters and line breaks into spaces, so that each update stays on one parseable line.

### Rotation

Log files are reopened for every line, so external tools like `logrotate` can move them at any time without signalling liquidsoap. Liquidsoap also rotates them the way icecast does: once a file grows past `<logsize>`, it is renamed to `<file>.old`, or to `<file>.YYYYmmdd_HHMMSS` when `<logarchive>` is set. Without `<logsize>`, the limit is 1GB.

## Listener Privacy

A listener's IP address is personal data. By default, `icecast.server` replaces it with a hash everywhere it is exposed: callbacks, `stats()`, the admin page and the logs. The hash is a 16-character MD5 digest of the address. It is the same on every run, so a listener can be followed across connections and restarts, for instance to count unique listeners.

This is a surface-level protection: it keeps addresses out of logs and dashboards. The key used by the default hash is public, so anyone can hash a guessed address and compare. If this matters to you, pass your own function with the `ip_hash` parameter, for instance with a secret key:

```{.liquidsoap include="icecast-server-ip-hash.liq"}

```

If you need the actual addresses and are allowed to keep them, you have to opt in explicitly:

```{.liquidsoap include="icecast-server-plain-ips.liq"}

```

Keep in mind that some data still contains raw addresses:

- Liquidsoap's own log mentions each listener's `ip:port` at level 4 (info) and above.
- The `x_forwarded_for` callback sees the raw address and headers, since it runs before hashing.

## `dedicated_encoder`

By default, all listeners on a mount share a single encoder instance. Setting `dedicated_encoder=true` allocates one independent encoder per listener.

With copy-based encoders (e.g. `copy_encoder("matroska")` or `%ffmpeg` with `%audio.copy` / `%video.copy`), this amounts to a lightweight remux per listener. The overhead is low — essentially one mux pass per listener — while the benefit is significant: each listener receives a clean, self-contained stream starting from a proper frame boundary.

```{.liquidsoap include="icecast-server-dedicated-encoder.liq"}

```

Each listener that connects will get their own stream, properly initialised from a clean frame boundary, regardless of when they join.

> **Note:** With full re-encoding (e.g. `%mp3`, `%aac`), `dedicated_encoder=true` creates a complete encoder per listener, which can be costly under load. Prefer copy-based encoders when using `dedicated_encoder`.

## Reverse Proxy and X-Forwarded-For

When `icecast.server` runs behind a reverse proxy (e.g. nginx), the listener IP recorded in stats will be the proxy's IP rather than the real client IP. Use `x_forwarded_for_proxy_ips` to fix this.

`x_forwarded_for_proxy_ips` takes a list of known proxy IPs. The default implementation (`icecast.server.x_forwarded_for`) walks the `X-Forwarded-For` header from right to left, skips any IP in the list, and returns the first non-proxy IP. This is safe against client spoofing: even if a client sends a forged `X-Forwarded-For` header, the proxy appends the real connecting IP as the rightmost entry, which is what gets used.

```liquidsoap
# Single nginx proxy at 10.0.0.1
icecast.server(
  x_forwarded_for_proxy_ips=["10.0.0.1"],
  password="hackme"
)

# CDN + nginx in front
icecast.server(
  x_forwarded_for_proxy_ips=["10.0.0.1", "203.0.113.42"],
  password="hackme"
)
```

For advanced use cases — custom header names, CIDR matching, CDN-specific headers like `CF-Connecting-IP` — pass a full callback via `x_forwarded_for`. The callback receives a record with `ip` (connection IP), `headers`, `protocol`, `uri`, and `proxy_ips` (from `x_forwarded_for_proxy_ips`, or `[]` if not set), and must return a string IP:

```liquidsoap
# Trust Cloudflare's CF-Connecting-IP header
icecast.server(
  x_forwarded_for=fun ({ip, headers, ...}) ->
    list.assoc(default=ip, "cf-connecting-ip", headers),
  password="hackme"
)
```

## Status Page

By default, `icecast.server` registers two HTTP endpoints on the icecast port:

- `/` — an HTML status page showing active mounts, listener counts, and playback controls
- `/status.json` — a JSON endpoint with the same data, polled by the HTML page every 5 seconds

### Disabling the Status Page

Pass `serve=false` to disable both endpoints:

```liquidsoap
icecast.server(serve=false, password="hackme")
```

### Access Control

Use `serve_auth` to gate both endpoints behind a check. The callback receives the HTTP request and must return `true` to allow access:

```{.liquidsoap include="icecast-server-serve-auth.liq"}

```

Any request that fails the check receives a `401 Unauthorized` response with a `WWW-Authenticate` header.

### Admin Listener Page

The public status page only shows listener counts. For operators, `icecast.server` also serves a listener page protected by HTTP Basic authentication:

- `/admin/listeners` — an HTML page listing each mount's listeners, with their (hashed) IP, user agent, connection time and bytes sent, plus the mount's peak listeners, connection count and total listening time
- `/admin/listeners.json` — the same data as JSON

The page is enabled by setting an admin password, either with the `admin_password` parameter or with `<admin-password>` in the configuration file. The user name defaults to `admin`. Without a password, the page is not registered at all. Like the status page, it is disabled by `serve=false`.

### Custom JSON Renderer

Use `serve_json` to replace the built-in `/status.json` output. The callback receives the stats list and must return a JSON string. The example below also disables the HTML page by returning a plain not-found response from `serve_html`:

```{.liquidsoap include="icecast-server-serve-json.liq"}

```

The stats list passed to both `serve_json` and `serve_html` is a list of `(mount, stats)` pairs where each `stats` record contains:

- `name`, `content_type`, `mime_type`: information about the mount's stream
- `started`: when the current source connected
- `listeners`: the connected listeners, as [listener records](#listener-callbacks)
- `peak_listeners`: the largest number of simultaneous listeners
- `connections`: the number of listener connections so far
- `listening_time`, `bytes_sent`: totals over finished listener sessions
- `current_metadata`: the mount's latest metadata

Listener records contain hashed IPs unless `ip_hash` says otherwise. Keep that in mind before publishing them from a custom renderer.

## Live Streaming Muxer Options

Some container formats require specific muxer flags to produce a valid live stream. For example, Matroska and WebM streams need their muxer told that there will be no seekable index at the end.

`icecast.server` automatically applies these options when remuxing an incoming stream via `copy_encoder`. The defaults are controlled by `settings.icecast.server.default_muxer_options`, which maps container format names to lists of FFmpeg muxer options:

| Format     | Default options    | Effect                                                              |
| ---------- | ------------------ | ------------------------------------------------------------------- |
| `matroska` | `dash=1`, `live=1` | Disables index/cues, writes streaming-compatible cluster timestamps |
| `webm`     | `dash=1`, `live=1` | Same as matroska (WebM is a subset)                                 |

To override the defaults globally, set the setting before starting the server:

```liquidsoap
settings.icecast.server.default_muxer_options :=
  [("matroska", [("dash", "1"), ("live", "1")]),
   ("webm",     [("dash", "1"), ("live", "1")])]
```

To override per-server instance, use the `format_options` parameter. The callback receives the detected container format name and returns the options list to apply:

```liquidsoap
# Disable all extra muxer options
icecast.server(format_options=fun (_) -> [], password="hackme")

# Custom options for matroska, defaults for everything else
icecast.server(
  format_options=fun (fmt) ->
    if fmt == "matroska" then [("dash", "1"), ("live", "1"), ("cluster_size_limit", "1000000")]
    else list.assoc(default=[], fmt, settings.icecast.server.default_muxer_options())
    end,
  password="hackme"
)
```

When `format_options` is `null` (the default), `settings.icecast.server.default_muxer_options` is used. When provided, it fully replaces the settings lookup — the callback is responsible for returning all options for every format.

## Key Differences from Icecast

The liquidsoap icecast server operates fundamentally differently from a traditional icecast server. Understanding these differences is important for getting the most out of this feature.

### Direct Encoded Content Manipulation

Unlike icecast, which primarily acts as a relay for encoded streams, liquidsoap can directly manipulate the encoded content. The incoming stream is demuxed, passed as encoded packets through the liquidsoap pipeline, and remuxed for output. This allows advanced format manipulation without ever needing to decode and re-encode, avoiding the CPU and memory consumption typically associated with transcoding.

This makes it possible to:

- **Format-compatible fallbacks**: When a source disconnects, the fallback mount seamlessly takes over without format incompatibility, because both streams go through the same processing pipeline.

- **Seamless transitions**: Listeners are never disconnected during source switches. The transition happens smoothly within the liquidsoap processing chain.

- **Per-listener clean streams**: With `dedicated_encoder=true` and copy encoders, each listener gets a fresh, properly initialised stream, enabling reliable playback of any streamable container format.

Because of this architecture, the following icecast options are fundamentally incompatible:

- `fallback-override`: In icecast, this allows a reconnecting source to "steal back" listeners from a fallback mount. In Liquidsoap, fallback is implemented by switching the underlying source inside the same output via `source.dynamic`. Listeners remain connected to the original mount throughout — there are no listeners "at the fallback mount" to reclaim, so the concept does not apply.

- `fallback-when-full`: In icecast, this redirects to a fallback when max-listeners is reached. Liquidsoap's architecture handles this differently through its own source management.

## Using an Icecast Configuration File

You can use a standard icecast XML configuration file:

```{.liquidsoap include="icecast-server-config.liq"}

```

This parses the configuration file and extracts supported settings. See the [Configuration Reference](#configuration-reference) section below for complete details on what is supported.

### Example Configuration File

```xml
<icecast>
    <limits>
        <sources>10</sources>
    </limits>

    <authentication>
        <source-password>mysecretpassword</source-password>
    </authentication>

    <listen-socket>
        <port>8000</port>
    </listen-socket>

    <http-headers>
        <header name="Access-Control-Allow-Origin" value="*" />
    </http-headers>

    <!-- Default settings for all mounts -->
    <mount type="default">
        <burst-size>65536</burst-size>
    </mount>

    <!-- Specific mount configuration -->
    <mount type="normal">
        <mount-name>/live.mp3</mount-name>
        <username>dj</username>
        <password>djpassword</password>
        <max-listeners>100</max-listeners>
        <fallback-mount>/fallback.mp3</fallback-mount>
        <on-connect>/path/to/script.sh</on-connect>
        <on-disconnect>/path/to/script.sh</on-disconnect>
    </mount>
</icecast>
```

## Configuration Reference

This section provides a comprehensive reference for icecast XML configuration options, indicating which are supported, which are not yet implemented, and which will likely never be supported due to architectural differences.

### listen-socket

| Option            | Status          | Notes                                                                                                          |
| ----------------- | --------------- | -------------------------------------------------------------------------------------------------------------- |
| `port`            | Supported       | Server listening port                                                                                          |
| `bind-address`    | Supported       | Sets `settings.harbor.bind_addrs`                                                                              |
| `tls` / `ssl`     | Supported       | Icecast 2.4: `0`/`1`. Icecast 2.5: `disabled`/`auto_no_plain`. Other 2.5 values log a warning and disable TLS. |
| `shoutcast-mount` | Not implemented | Shoutcast compatibility mount                                                                                  |

Note: Only the first `listen-socket` entry is used. Multiple listen sockets are not supported; a warning is logged if more than one is found.

### authentication

| Option            | Status    | Notes                                     |
| ----------------- | --------- | ----------------------------------------- |
| `source-password` | Supported | Global password for source clients        |
| `admin-user`      | Supported | User name for the admin listener page     |
| `admin-password`  | Supported | Password enabling the admin listener page |

### limits

| Option             | Status        | Notes                                             |
| ------------------ | ------------- | ------------------------------------------------- |
| `sources`          | Supported     | Maximum number of simultaneous source connections |
| `clients`          | Parsed (TODO) | Maximum number of listeners (not yet enforced)    |
| `source-timeout`   | Supported     | Timeout in seconds for source connections (float) |
| `client-timeout`   | Supported     | Timeout in seconds for client connections (float) |
| `burst-size`       | Supported     | Default burst size for new listeners (bytes)      |
| `burst-on-connect` | Supported     | Set to 0 to disable burst entirely                |
| `queue-size`       | Not used      | Liquidsoap manages queues differently             |
| `header-timeout`   | Not used      | Liquidsoap handles headers differently            |

### paths

| Option                                | Status          | Notes                                                                                     |
| ------------------------------------- | --------------- | ----------------------------------------------------------------------------------------- |
| `basedir`                             | Not implemented |                                                                                           |
| `logdir`                              | Supported       | Directory of the log files set in `<logging>`                                             |
| `pidfile`                             | Not supported   |                                                                                           |
| `tls-certificate` / `ssl-certificate` | Supported       | Path to TLS certificate file (required when TLS is enabled). May include the private key. |
| `tls-key`                             | Supported       | Path to separate TLS private key file (icecast 2.5 only)                                  |
| `webroot`                             | Not implemented | No built-in web interface                                                                 |
| `adminroot`                           | Not implemented | No built-in admin interface                                                               |
| `allow-ip`                            | Not implemented | Use a reverse proxy (nginx) or firewall instead                                           |
| `deny-ip`                             | Not implemented | Use a reverse proxy (nginx) or firewall instead                                           |
| `ssl-allowed-ciphers`                 | Not implemented | TLS cipher configuration not exposed                                                      |
| `alias`                               | Not implemented | URL aliasing not supported                                                                |

### logging

Log files are created in `<paths><logdir>`. Without a `logdir`, only the `-` value (console output) takes effect.

| Option          | Status          | Notes                                                                                            |
| --------------- | --------------- | ------------------------------------------------------------------------------------------------ |
| `accesslog`     | Supported       | Access log file, `access.log` by default. `-` writes to standard error. See [above](#access-log) |
| `errorlog`      | Supported       | Liquidsoap's own log file, `error.log` by default. `-` logs to the console only                  |
| `playlistlog`   | Supported       | Playlist log file, disabled by default. See [above](#playlist-log)                               |
| `loglevel`      | Supported       | `1`/`error` to `4`/`debug`, mapped to liquidsoap log levels 2 to 5                               |
| `logsize`       | Supported       | Size in KiB past which access and playlist logs are rotated, 1GB by default                      |
| `logarchive`    | Supported       | When `1`, rotated logs keep a timestamped name instead of replacing `<file>.old`                 |
| `memorybacklog` | Not implemented | Icecast's in-memory log view is not available                                                    |

The `accesslog` and `playlistlog` settings are overridden by the `access_log` and `playlist_log` parameters.

### http-headers

Global HTTP headers are fully supported. Use the standard icecast format:

```xml
<http-headers>
    <header name="Access-Control-Allow-Origin" value="*" />
    <header name="X-Custom-Header" value="value" />
</http-headers>
```

### mount

Mount configurations support both `type="default"` (settings applied to all mounts) and `type="normal"` (specific mount configurations).

#### Supported Mount Options

| Option           | Description                                        |
| ---------------- | -------------------------------------------------- |
| `mount-name`     | Mount point path (e.g., `/live.mp3`)               |
| `username`       | Source client username (default: "source")         |
| `password`       | Mount-specific password (overrides global)         |
| `dump-file`      | Path to dump the raw stream to a file              |
| `burst-size`     | Initial burst size for new listeners (bytes)       |
| `fallback-mount` | Mount to fall back to when this source disconnects |
| `on-connect`     | Shell command to execute when source connects      |
| `on-disconnect`  | Shell command to execute when source disconnects   |
| `http-headers`   | Custom HTTP headers for this mount's responses     |

#### Unsupported Mount Options

These options are not yet implemented:

| Option                  | Reason                                         |
| ----------------------- | ---------------------------------------------- |
| `max-listeners`         | Not yet implemented                            |
| `hidden`                | Not yet implemented                            |
| `public`                | Directory listing registration not implemented |
| `intro`                 | Intro file playback not implemented            |
| `max-listener-duration` | Listener duration limits not implemented       |
| `authentication`        | URL-based authentication not implemented       |

#### Incompatible Mount Options

These options are fundamentally incompatible with liquidsoap's architecture.

| Option               | Reason                                                                                       |
| -------------------- | -------------------------------------------------------------------------------------------- |
| `fallback-override`  | Liquidsoap manages sources through its own pipeline; "stealing back" listeners doesn't apply |
| `fallback-when-full` | Liquidsoap handles listener limits through its own source management                         |

### Configuration Sections Not Supported

The following icecast configuration sections are not supported and will be ignored:

- `fileserve` - Static file serving (use liquidsoap's harbor HTTP handlers)
- `relay` - Stream relaying (use liquidsoap's `input.http` instead)
- `directory` - Directory listings (YP)
- `security` - Use liquidsoap's security settings

## Complete Example

Here's a complete example showing a radio station setup with multiple mounts and fallback handling:

```{.liquidsoap include="icecast-server-complete.liq"}

```

### Minimal Configuration File

```xml
<icecast>
    <authentication>
        <source-password>changeme</source-password>
    </authentication>

    <listen-socket>
        <port>8000</port>
    </listen-socket>

    <mount type="default">
        <burst-size>65536</burst-size>
        <fallback-mount>/fallback</fallback-mount>
    </mount>

    <mount type="normal">
        <mount-name>/live</mount-name>
    </mount>
</icecast>
```

## See Also

- [Harbor Input](./harbor.md): The underlying technology for receiving source connections
- [Harbor HTTP](./harbor_http.md): HTTP interface capabilities
- [HLS Output](./hls_output.md): Alternative streaming output method
