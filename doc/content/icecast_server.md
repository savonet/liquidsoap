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
- `config`: Optional path to an icecast XML configuration file
- `dedicated_encoder`: Allocate one encoder per listener (see [below](#dedicated_encoder))
- `serve`: Enable or disable the built-in status page (default: `true`, see [below](#status-page))
- `serve_auth`: Optional callback `(request) -> bool` for access control on status endpoints
- `serve_json`: Optional callback `(stats) -> string` to replace the built-in JSON renderer
- `serve_html`: Optional callback `(stats) -> string` to replace the built-in HTML renderer
- `x_forwarded_for_proxy_ips`: List of known reverse-proxy IPs for real-IP extraction (see [below](#reverse-proxy-and-x-forwarded-for))
- `x_forwarded_for`: Advanced callback to fully override real-IP extraction logic (see [below](#reverse-proxy-and-x-forwarded-for))
- `format_options`: Optional callback `(string) -> [(string * string)]` that returns muxer options for a given container format name. When `null`, falls back to `settings.icecast.server.default_muxer_options` (see [below](#live-streaming-muxer-options))

### Return Value

The `icecast.server` function returns a record with the following methods:

- `mounts()`: Returns a list of currently active mount points
- `get_source(mount)`: Returns the source for a given mount point
- `get_config(mount)`: Returns the current `{format, streams}` record for a mount, or `null` if the mount is not active
- `stats()`: Returns the current mount stats list (same data as the JSON status endpoint)
- `on_connect(handler)`: Register a handler called when a source connects
- `on_disconnect(handler)`: Register a handler called when a source disconnects

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

### Custom JSON Renderer

Use `serve_json` to replace the built-in `/status.json` output. The callback receives the stats list and must return a JSON string. The example below also disables the HTML page by returning a plain not-found response from `serve_html`:

```{.liquidsoap include="icecast-server-serve-json.liq"}

```

The stats list passed to both `serve_json` and `serve_html` is a list of `(mount, stats)` pairs where each `stats` record contains: `name`, `content_type`, `mime_type`, `started`, `listeners`, `peak_listeners`, and `current_metadata`.

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

| Option            | Status    | Notes                              |
| ----------------- | --------- | ---------------------------------- |
| `source-password` | Supported | Global password for source clients |

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
| `logdir`                              | Supported       | Enables log file and sets path to `#{logdir}/<script>.log`                                |
| `pidfile`                             | Supported       | Enables pidfile and sets path to the value                                                |
| `tls-certificate` / `ssl-certificate` | Supported       | Path to TLS certificate file (required when TLS is enabled). May include the private key. |
| `tls-key`                             | Supported       | Path to separate TLS private key file (icecast 2.5 only)                                  |
| `webroot`                             | Not implemented | No built-in web interface                                                                 |
| `adminroot`                           | Not implemented | No built-in admin interface                                                               |
| `allow-ip`                            | Not implemented | Use a reverse proxy (nginx) or firewall instead                                           |
| `deny-ip`                             | Not implemented | Use a reverse proxy (nginx) or firewall instead                                           |
| `ssl-allowed-ciphers`                 | Not implemented | TLS cipher configuration not exposed                                                      |
| `alias`                               | Not implemented | URL aliasing not supported                                                                |

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
- `logging` - Use liquidsoap's logging settings
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

- [Harbor Input](harbor.html): The underlying technology for receiving source connections
- [Harbor HTTP](harbor_http.html): HTTP interface capabilities
- [HLS Output](hls_output.html): Alternative streaming output method
