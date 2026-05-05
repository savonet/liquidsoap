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
- `prepare`: Optional functions to customize encoder and fallback creation (see [below](#prepare))
- `dedicated_encoder`: Allocate one encoder per listener (see [below](#dedicated_encoder))
- `serve`: Enable or disable the built-in status page (default: `true`, see [below](#status-page))
- `serve_auth`: Optional callback `(request) -> bool` for access control on status endpoints
- `serve_json`: Optional callback `(stats) -> string` to replace the built-in JSON renderer
- `serve_html`: Optional callback `(stats) -> string` to replace the built-in HTML renderer

### Return Value

The `icecast.server` function returns a record with the following methods:

- `mounts()`: Returns a list of currently active mount points
- `get_source(mount)`: Returns the source for a given mount point
- `on_connect(handler)`: Register a handler called when a source connects
- `on_disconnect(handler)`: Register a handler called when a source disconnects

## `prepare`

The `prepare` parameter is a function that receives the full context for a new mount and is responsible for creating the output — typically by calling `output.harbor`. When `null`, a default output is created using `copy_encoder` and a standard `fallback`.

```liquidsoap
prepare({
  source,            # source  — the main (live) source
  fallback,          # source? — the fallback source, or null if no fallback mount is configured
  copy_encoder,      # (string?) -> encoder — creates a copy encoder for the given format
  format,            # string? — content-type or format name of the incoming stream
  streams,           # list of stream descriptions (each has .type, .codec, and format-specific fields)
  source_headers,    # [(string, string)] — HTTP headers sent by the source client
  mount,             # string  — mount point path
  headers,           # [(string, string)] — HTTP response headers for listeners
  burst,             # int?   — initial burst size for new listeners
  timeout,           # float  — client timeout in seconds
  port,              # int    — listening port
  transport,         # http transport (plain or TLS)
  dumpfile,          # string? — path to dump the raw stream, or null
  dedicated_encoder  # bool   — whether to allocate one encoder per listener
}) -> {clock, shutdown}
```

The function must return a record with at least `clock` (used to synchronise the server clock) and `shutdown`. `output.harbor` returns this directly.

### Example: Adding a video stream when the sender sends audio only

Both `prepare.encoder` and `prepare.fallback` receive the stream descriptions via `streams` / `config.streams`, so both can independently check whether the source carries video:

```{.liquidsoap include="icecast-server-prepare-video.liq"}

```

### Example: Remuxing to a different container format

When you only want to change the container format without touching the encoded data, pass a `prepare.encoder` that calls `copy_encoder` with the target format name. No `prepare.fallback` override is needed:

```{.liquidsoap include="icecast-server-prepare-remux.liq"}

```

This remuxes any incoming stream into an Ogg container on the fly. For example, a source sending plain FLAC will be transparently remuxed into Ogg/FLAC before being delivered to listeners.

## `dedicated_encoder`

By default, all listeners on a mount share a single encoder instance. Setting `dedicated_encoder=true` allocates one independent encoder per listener.

With copy-based encoders (e.g. `copy_encoder("matroska")` or `%ffmpeg` with `%audio.copy` / `%video.copy`), this amounts to a lightweight remux per listener. The overhead is low — essentially one mux pass per listener — while the benefit is significant: each listener receives a clean, self-contained stream starting from a proper frame boundary. This makes it possible to stream any container format (MKV, MPEG-TS, WebM, …) to any listener from any point in the stream, without the hacks typically needed for formats like Ogg that embed framing headers.

For example, Matroska (`"matroska"`) and MPEG-TS (`"mpegts"`) both require `dedicated_encoder=true`: without it, listeners joining mid-stream will not receive the initialisation headers needed to decode the stream.

```{.liquidsoap include="icecast-server-dedicated-encoder.liq"}

```

Each listener that connects will get its own MKV stream, properly initialised, regardless of when they join.

> **Note:** With full re-encoding (e.g. `%mp3`, `%aac`), `dedicated_encoder=true` creates a complete encoder per listener, which can be costly under load. Prefer copy-based encoders when using `dedicated_encoder`.

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

## Key Differences from Icecast

The liquidsoap icecast server operates fundamentally differently from a traditional icecast server. Understanding these differences is important for getting the most out of this feature.

### Direct Encoded Content Manipulation

Unlike icecast, which primarily acts as a relay for encoded streams, liquidsoap can directly manipulate the encoded content. The incoming stream is demuxed, passed as encoded packets through the liquidsoap pipeline, and remuxed for output. This allows advanced format manipulation without ever needing to decode and re-encode, avoiding the CPU and memory consumption typically associated with transcoding.

This makes it possible to:

- **Format-compatible fallbacks**: When a source disconnects, the fallback mount seamlessly takes over without format incompatibility, because both streams go through the same processing pipeline.

- **Seamless transitions**: Listeners are never disconnected during source switches. The transition happens smoothly within the liquidsoap processing chain.

- **Per-listener clean streams**: With `dedicated_encoder=true` and copy encoders, each listener gets a fresh, properly initialised stream, enabling reliable playback of any streamable container format.

Because of this architecture, the following icecast options are fundamentally incompatible:

- `fallback-override`: In icecast, this allows a reconnecting source to "steal back" listeners from a fallback mount. Since liquidsoap manages sources through its own pipeline, this concept doesn't apply the same way.

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

| Option            | Status          | Notes                                                          |
| ----------------- | --------------- | -------------------------------------------------------------- |
| `port`            | Supported       | Server listening port                                          |
| `bind-address`    | Supported       | Sets `settings.harbor.bind_addrs`                              |
| `tls`             | Supported       | Set to `1` to enable TLS (requires `tls-certificate` in paths) |
| `shoutcast-mount` | Not implemented | Shoutcast compatibility mount                                  |

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

| Option                | Status          | Notes                                                      |
| --------------------- | --------------- | ---------------------------------------------------------- |
| `basedir`             | Not implemented |                                                            |
| `logdir`              | Supported       | Enables log file and sets path to `#{logdir}/<script>.log` |
| `pidfile`             | Supported       | Enables pidfile and sets path to the value                 |
| `tls-certificate`     | Supported       | Path to TLS certificate (required when tls=1)              |
| `webroot`             | Not implemented | No built-in web interface                                  |
| `adminroot`           | Not implemented | No built-in admin interface                                |
| `allow-ip`            | Not implemented | Use a reverse proxy (nginx) or firewall instead            |
| `deny-ip`             | Not implemented | Use a reverse proxy (nginx) or firewall instead            |
| `ssl-allowed-ciphers` | Not implemented | TLS cipher configuration not exposed                       |
| `alias`               | Not implemented | URL aliasing not supported                                 |

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
