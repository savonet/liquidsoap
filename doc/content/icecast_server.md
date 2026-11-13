# Icecast Server

Liquidsoap can be the Icecast server itself. `icecast.server` accepts source clients such as butt, Mixxx or another liquidsoap, serves their streams to listeners, and speaks the protocols that players, statistics tools and listener authentication backends written for Icecast, RSAS or Icecast-KH expect. You can also feed its mounts from your own liquidsoap sources. To send your stream to an external Icecast server instead, see [streaming to Icecast and Shoutcast](./icecast.md).

## Starting a server

A server is described by one record, its _parameters_. Every field is optional and takes its default when missing, so a server needs little more than a source password. Here it also logs its sources coming and going:

```{.liquidsoap include="icecast-server-basic.liq"}

```

This listens on port 8000 for source clients and listeners, and serves the Shoutcast source protocol on port 8001. A source client logs in with the user `source` and the `source_password`. It can then stream to any mount, for instance `/live.mp3`, and listeners find it at `http://localhost:8000/live.mp3`.

Source clients connect with HTTP `PUT` or `POST`, Icecast's legacy `SOURCE` method, a WebSocket, or the Shoutcast protocol, which feeds the mount named by `shoutcast_mount`. Some paths are never mounts because the server answers them itself: `/`, `/status.json`, `/status-json.xsl`, `/metrics`, `/admin`, `/admin.cgi`, anything under `/admin/`, and anything ending in `/health` or `/manage`.

The main server parameters are:

| Field             | Default            | Meaning                                                                 |
| ----------------- | ------------------ | ----------------------------------------------------------------------- |
| `port`            | `8000`             | Port for sources and listeners. The Shoutcast protocol uses `port + 1`. |
| `bind_address`    | all interfaces     | Local address to bind.                                                  |
| `transport`       | plain HTTP         | The HTTP transport of the port, for instance TLS (see [below](#tls)).   |
| `compatibility`   | `"icecast-2.5"`    | Which implementation to mimic (see [below](#compatibility)).            |
| `hostname`        | `"localhost"`      | The name the server gives itself in status pages and to backends.       |
| `source_password` | `"hackme"`         | Password of source clients when their mount has none of its own.        |
| `shoutcast_mount` | `"/stream"`        | Mount fed by a Shoutcast source client.                                 |
| `max_sources`     | unlimited          | Most simultaneous source clients.                                       |
| `max_clients`     | unlimited          | Most simultaneous listeners over all mounts.                            |
| `source_timeout`  | `10.`              | Seconds of silence after which a source client is dropped.              |
| `client_timeout`  | `30.`              | Seconds a listener's connection may stall before it is dropped.         |
| `burst_size`      | `65536`            | Bytes sent at once to a new listener, so that playback starts quickly.  |
| `name`            | `"icecast.server"` | Label of the server in logs.                                            |

The server warns while a mount accepts the default `hackme` password.

### The server value

`icecast.server` returns a value with these methods:

- `on_source_connect`, `on_source_disconnect`, `on_listener_connect`, `on_listener_disconnect` and `on_metadata` register handlers for events (see [below](#events));
- `stats()` returns a snapshot of the server's statistics;
- `reload(parameters)` replaces the parameters of the running server (see [below](#reloading));
- `reopen_logs()` reopens the log files after an external tool moved them;
- `attach_source(~format, ~headers, mount, source)` and `detach_source(mount)` feed a mount from a liquidsoap source (see [below](#feeding-a-mount-from-liquidsoap)).

## Mounts

Most stations have more than one mount: the live show, an automated backup, a members-only stream, a low-bitrate version. Each needs its own credentials, limits or fallback. All of this comes from one callback, `mount`: the server calls it with a mount's path, and it answers the parameters of that mount, or `null` when the mount has none of its own:

```{.liquidsoap include="icecast-server-mounts.liq"}

```

Since it is a function, wildcards, defaults shared by several mounts or a lookup in your own table are just code. A source client may create any mount the other rules allow, as on Icecast: the callback configures mounts, it does not declare them.

| Field                   | Meaning                                                                                                  |
| ----------------------- | -------------------------------------------------------------------------------------------------------- |
| `source_user`           | User name source clients present, `"source"` by default.                                                 |
| `source_password`       | Password of the mount's source clients, the server's by default.                                         |
| `source_decision`       | A function `(request) -> bool` replacing the user and password check of source clients.                  |
| `on_connect`            | Called in the background when something starts feeding the mount.                                        |
| `on_disconnect`         | Called in the background when it stops.                                                                  |
| `max_listeners`         | Most simultaneous listeners on the mount.                                                                |
| `listener_decision`     | Decides each listener (see [below](#deciding-on-listeners)).                                             |
| `max_listener_duration` | Seconds after which a listener is disconnected.                                                          |
| `fallback`              | What feeds the mount while it has no source: `{mount = "/other.mp3"}` or `{source = s}`.                 |
| `fallback_when_full`    | Redirect listeners past `max_listeners` along the fallback mounts, with `307`, instead of refusing them. |
| `linger`                | Seconds the mount keeps its listeners, waiting, when its source drops and nothing else feeds it.         |
| `hidden`                | Keep the mount off the public status pages.                                                              |
| `max_history`           | Titles kept in the mount's history, 10 by default.                                                       |
| `health_password`       | Password of the mount's `/<mount>/health` page.                                                          |
| `burst_size`            | The mount's own burst. `0` disables it whatever the server's value.                                      |
| `dedicated_encoder`     | Give each listener its own encoder, whatever the server's value.                                         |
| `http_headers`          | Headers added to the mount's listener replies (see [below](#http-headers)).                              |
| `dump_file`             | File the mount's stream is recorded to, a `strftime` pattern expanded at each recording start.           |
| `dump_file_size_limit`  | Bytes after which a recording stops.                                                                     |
| `dump_file_time_limit`  | Seconds after which a recording stops.                                                                   |

### Fallbacks

Liquidsoap does not just relay the bytes it receives: it reads the stream it serves. When a mount loses its source, it switches to its fallback at the next packet, and back to its own source as soon as that one returns. Listeners never leave their mount: their connection, headers and limits stay the same, and the switch is invisible to their player. A fallback mount without a source of its own passes on to its own fallback, up to ten mounts.

A fallback must carry the same format as the mount. A mount with no fallback that loses its source keeps its listeners for `linger` seconds, so that a DJ whose connection blips does not lose the audience.

### Feeding a mount from liquidsoap

A mount does not need a source client. `attach_source` feeds it from a liquidsoap source of encoded content, so that your automation is served next to the live mounts:

```{.liquidsoap include="icecast-server-attach.liq"}

```

`format` is the encoder that writes the content for listeners, here a copy of the MP3 data, and `headers` are the stream identity headers (`ice-name`, `ice-genre`, …) a source client would have sent. The same kind of source can be a mount's `fallback`, as in the [complete example](#a-complete-example).

## Deciding on listeners

Paid streams, geo-restricted content, a members area or a per-listener ad all need a decision on each listener before the stream starts. A mount's `listener_decision` is a function from the listener's request to an outcome, a record whose fields are all optional:

| Field        | Meaning                                                                            |
| ------------ | ---------------------------------------------------------------------------------- |
| `refuse`     | Refuse the listener, or redirect it, with `icecast.server.refusal` or `.redirect`. |
| `time_limit` | Seconds after which the listener is disconnected.                                  |
| `preroll`    | Bytes sent before the stream (see [below](#prerolls)).                             |
| `exempt`     | Skip `max_listeners`, for instance for a trusted relay.                            |
| `on_release` | Called once when the listener leaves.                                              |
| `user`       | A new user name for the listener.                                                  |

The request carries the listener's `address`, `headers`, `user` and `password` (from HTTP Basic credentials), `mount`, `query`, the mount's current `listeners`, and `user_listeners(user)`, the number of listeners that user already has on the mount.

The standard library has ready-made decisions, and `icecast.server.decision.all` composes several in order, the first refusal winning:

```{.liquidsoap include="icecast-server-decisions.liq"}

```

- `icecast.server.decision.access_lists(~deny, ~allow, ~agents)` refuses addresses from a deny file, addresses missing from an allow file, and `User-Agent` values from an agents file. Lines are addresses or wildcard patterns.
- `icecast.server.decision.htpasswd(file)` checks HTTP Basic credentials against a file of `user:md5` lines. Without valid credentials the listener is refused with `401`, which makes a browser ask for them.
- `icecast.server.decision.url_auth(~cache, options)` asks a web service of yours, with Icecast's protocol (see below).

The files are read again when they change.

### URL authentication

Icecast leaves the decision to a web service, and `icecast.server` speaks the same protocol, extended the way RSAS and Icecast-KH extend it, so that an existing backend keeps working:

```{.liquidsoap include="icecast-server-url-auth.liq"}

```

For each listener, the server POSTs Icecast's form fields to `listener_add`: `action`, `server`, `port`, `client`, `mount`, `user`, `pass`, `ip`, `agent`, then the fields RSAS and Icecast-KH add. The reply admits the listener with `icecast-auth-user: 1` (or `x-icecast-auth-result: ok`), and can set a time limit, redirect the listener, refuse it with its own status and message, or choose a preroll. A request that fails refuses the listener with `401`, unless `presume_innocent` is set. When an admitted listener leaves, the server POSTs to `listener_remove` with the session's duration, which is what billing and concurrent-session tracking need.

The options keep Icecast's names: `listener_add`, `listener_remove`, `username`, `password`, `auth_header`, `timelimit_header`, `header_auth`, `header_timelimit`, `header_message`, `header_alter_action`, `header_alter_argument`, `action_add`, `action_remove`, `headers`, `header_prefix`, `timeout`, `presume_innocent` and `on_error_wait`.

### Listener limits

A listener past the mount's `max_listeners` or the server's `max_clients` is refused with the status the chosen implementation sends: `503 Service Unavailable` under `"icecast-2.5"` and `"rsas"`, `403 Forbidden` under `"icecast-2.4"` and `"kh"`. Players retry on `503` and not on `403`, so some tools need the older one:

```{.liquidsoap include="icecast-server-status-codes.liq"}

```

### Prerolls

A preroll is a short file a listener hears before joining the stream: a station jingle, a sponsor message, or an ad an ad server chose for that listener. A decision gives it as bytes, and `icecast.server.preroll_cache` loads them from a local file or a URL:

```{.liquidsoap include="icecast-server-preroll.liq"}

```

The file is sent as it is, before the stream, so it must use the stream's codec and parameters: for an MP3 stream, an MP3 file with the same sample rate and channels. Files are kept in memory and shared by every listener, up to `settings.icecast.server.preroll_cache_size` bytes in all, the least recently used dropped first. A local file is read again when it changes, and a URL is checked again after two hours using its ETag. A file that cannot be loaded is logged, and the listener gets the stream without a preroll.

## Events

Listener statistics, royalty reports, dashboards and notifications are all built on knowing who connects and when. The server reports each event to the handlers you register:

```{.liquidsoap include="icecast-server-listener-callbacks.liq"}

```

Listener handlers receive a record with `id`, `mount`, `ip` (hashed, see [privacy](#listener-privacy)), `user`, `user_agent`, `referer`, `uri`, `protocol`, `connected_at`, `duration` and `bytes_sent`: zero on connect, the session's totals on disconnect. Source handlers receive `mount`, `ip` (the source client's address, not hashed), `agent`, `server` (`{hostname, port, instance}`) and `compatibility`, and on connect the liquidsoap `source` feeding the mount, its `format` and the `headers` the source client sent. `on_metadata` handlers receive `mount`, `metadata` and the mount's `listeners`.

Every method takes `synchronous`, `false` by default. An asynchronous handler runs in its own thread and never delays the stream, but no order is guaranteed between such handlers, so counters they share must use `atomic`. A synchronous handler runs in order with the event and must be short. Every admitted listener produces exactly one connect and one disconnect event.

## Status pages and statistics

The server answers a few pages on its own port:

- `/`: an HTML page with each mount's name, format, now playing, listener count and a player;
- `/status.json`: the same data as JSON;
- `/status-json.xsl`: Icecast's JSON, which players and website widgets read;
- `/health` and `/<mount>/health`: RSAS's health document, protected by `health_password` when set;
- `/metrics`: a Prometheus export, when `metrics` is `true`.

They show counts only, never listener addresses, and leave out hidden mounts. `status_page = false` turns off the first three, `/health` and `/<mount>/health`; under `"rsas"` the health pages stay, as RSAS serves them on their own. `status_access` gates the first three and `/metrics` behind a check of your own:

```{.liquidsoap include="icecast-server-serve-auth.liq"}

```

`status_json` and `status_html` replace the rendering of `/status.json` and `/`. Each receives the `stats()` snapshot, hidden mounts included, so a public page filters them on `hidden`. Its `mounts` hold, per mount, `mount`, `name`, `listeners`, `peak_listeners`, `connections`, `listening_time`, `bytes_sent`, `metadata`, `history` and the stream's identity:

```{.liquidsoap include="icecast-server-serve-json.liq"}

```

With an `admin_password` (user `admin_user`, `"admin"` by default), the server also serves the admin pages: `/admin/listeners` is a page to watch each mount and its listeners, update a mount's metadata or disconnect its source, `/admin/listeners.json` lists each mount's listeners, and `/admin/stats`, `/admin/listmounts` and `/admin/listclients` answer Icecast's XML for administration tools. The admin commands `metadata`, `killsource` and `fallbacks` work as on Icecast.

## Log files

Log analyzers and statistics tools read the files Icecast writes. `icecast.server` writes the same access, playlist and error logs:

```{.liquidsoap include="icecast-server-access-log.liq"}

```

- The **access log** gets one line per listener session, in Icecast's combined log format with the session length appended.
- The **playlist log** gets one line per metadata change: time, mount, listener count and `artist - title`.
- The **error log** gets the server's own events, in Icecast's format, levels and categories, up to `log_level` (on liquidsoap's scale: 4, info, by default). Every event also goes to liquidsoap's own log.

`"-"` writes to standard error. Files rotate past `log_size` bytes, to `<file>.old`, or to a timestamped name with `log_archive`. After `logrotate` moves a file, `reopen_logs()` opens it again at its path. The server takes no signal itself: the example binds `SIGHUP` with `on_signal`. A setup that also reloads on `SIGHUP` reopens the logs in the same handler, as the [reload example](#reloading) does, since a reload keeps files whose path did not change open.

## Listener privacy

A listener's address is personal data. The server applies `address_hash` to every listener address before anything sees it: event handlers, `stats()`, the admin pages and the logs. Source client addresses are the operator's own side and are not hashed. The default is a 16-character MD5 digest, the same on every run, so that you can still count unique listeners. Its key is public, so anyone can hash a guessed address and compare. A secret key protects them better:

```{.liquidsoap include="icecast-server-ip-hash.liq"}

```

If you need the plain addresses and are allowed to keep them, opt in explicitly:

```{.liquidsoap include="icecast-server-plain-ips.liq"}

```

Listener decisions and URL authentication backends receive the plain address, as on Icecast.

### Behind a reverse proxy

Behind a proxy such as nginx, every listener connects from the proxy's address. List your proxies in `proxy_ips`, as addresses or CIDR ranges, and the server takes the listener's address from `X-Forwarded-For`, walking it from the right and skipping trusted proxies. An empty list trusts the rightmost entry. For other headers, `forwarded_address` is a function of the request that answers the address:

```{.liquidsoap include="icecast-server-forwarded.liq"}

```

## HTTP headers

`http_headers`, on the server and per mount, adds headers to listener replies. An entry is `{name, value}`, or `{name, compute}`, where `compute` is a function of the request and the reply's status, for headers that depend on them. An entry with a `status` applies only to replies with that status. A reply's list is, in order, the listener decision's headers, the stream identity, the server's entries, then the mount's, and the first entry of a name wins: a mount cannot override a header the server sets. `icecast.server.headers.cors(~origins, ~expose)` builds the CORS headers web players need:

```{.liquidsoap include="icecast-server-headers.liq"}

```

## TLS

TLS is a property of the port's transport. Pass one built with `http.transport.tls` or `http.transport.ssl`:

```{.liquidsoap include="icecast-server-tls.liq"}

```

The transport reads its certificate again at each `reload` and once a day on its own, so that a renewed certificate is used without a restart. See [Renewing certificates](./harbor.md#renewing-certificates).

## Encoding and muxing

By default, the listeners of a mount share one encoder. `dedicated_encoder = true` gives each listener its own. With copy encoders, such as `%ffmpeg(format = "matroska", %audio.copy)`, this is a cheap remux, and each listener gets a stream that starts with its own container header, so that listeners can join any streamable container at any time. With full re-encoding, it costs one encoder per listener. A mount's own `dedicated_encoder` wins over the server's:

```{.liquidsoap include="icecast-server-dedicated-encoder.liq"}

```

Some containers need options to be written live: Matroska and WebM get `live=1` by default. `muxer_options` is a function from a container name to the options to use, and `settings.icecast.server.default_muxer_options` holds the defaults.

## Compatibility

Icecast 2.4, Icecast 2.5, RSAS and Icecast-KH differ in small, visible ways: the status of a refused listener, the details of URL authentication, the status pages, the CORS headers. `compatibility` chooses which one the server mimics: `"icecast-2.5"` (default), `"icecast-2.4"`, `"rsas"` or `"kh"`. Wherever they differ, the chosen one wins. A behaviour only another implementation has, and that conflicts with nothing, is kept too, so that a backend written for any of them keeps working.

`compatibility` is read once, at startup. A reload with another value keeps the running one and logs it.

Some behaviours are the same under every value, because the server reads the streams it serves: listeners stay on their mount through fallbacks and come back with their source, a mount keeps its listeners for `linger` seconds through a source drop, one `transport` replaces Icecast's TLS modes, and `proxy_ips` takes CIDR ranges.

## Reloading

A running server usually has sources and listeners connected, and restarting it to change a password or a fallback would drop all of them. `reload(parameters)` replaces the whole record in place. The new record is checked first: when it is invalid, `reload` raises `error.icecast.server.invalid_parameters` and nothing changes. Otherwise it applies in one step, and no source or listener is disconnected.

Build the record in one function, called for the first start and for every reload, so that each reload computes it whole, overrides included:

```{.liquidsoap include="icecast-server-reload.liq"}

```

Parameters apply at different times:

- `name`, `port`, `bind_address`, `transport` and `compatibility` are read at startup only. A reload that changes the name, port, address or compatibility logs that it was not applied.
- `client_timeout`, `burst_on_connect`, `burst_size`, `http_headers`, `dedicated_encoder` and `muxer_options` apply when a mount output is next created. A running mount keeps the values it started with.
- Everything else applies at its next use: the next listener, the next source connection, the next log line, the next event. A live mount follows a changed `fallback` at once, and one left with nothing to send is shut down.

Create liquidsoap sources once, outside the function: a mount follows its fallback source by identity, and a source rebuilt at each reload would be a new one. Handlers registered with the `on_*` methods are kept by a reload. `event_bindings`, which are part of the record, are replaced.

## Using an Icecast configuration file

`icecast.server.config.load(path)` turns an Icecast, RSAS or Icecast-KH XML configuration file into parameters, so that an existing setup moves over as it is:

```{.liquidsoap include="icecast-server-config.liq"}

```

The file is one way to build the record: you can change the result like any other record, for instance `icecast.server.config.load(path).{port = 8100}`, and reload it as [above](#reloading). The `compatibility` argument (`"icecast-2.5"` by default) is written into the record and also decides how mounts are matched and whether the `cors` header type exists. No element of the file sets it, because most files are valid for several implementations.

A mount's settings come from the record's `mount` function, which the file fills in. To change one mount, wrap that function. It returns `null` for a path the file has no `<mount>` for, so start from `m ?? {}` to cover that case too:

```{.liquidsoap include="icecast-server-config-mount.liq"}

```

The translation is best effort. It maps the common settings and ignores, with a warning naming it, every element it does not reproduce: the other settings keep working. A file that would be served less safely is rejected instead, with `error.icecast.server.invalid_parameters`:

- malformed XML, a root other than `<icecast>`, or a value of the wrong kind, such as a non-numeric `<port>`;
- a mount `<authentication>` other than `url` or `htpasswd`, or one using `<role>`: ignoring it would open the mount to everyone;
- a `<tls>` mode that serves TLS and plain HTTP on one port (`auto`, `auto_no_plain`, `rfc2817`), or a certificate with no `<tls>` mode, which Icecast takes to mean the same. Set `<tls>` to `rfc2818` for TLS or `disabled` for plain HTTP.

### What is translated

| Element                                                                                                                    | Becomes                                                                                                      |
| -------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| `hostname`, `location`, `admin`, `server-id`                                                                               | `hostname`, `location`, `admin_contact`, `server_id`                                                         |
| `listen-socket`: `port`, `bind-address`, `tls` or `ssl`, `shoutcast-mount`, `trusted-proxy`                                | `port`, `bind_address`, `transport`, `shoutcast_mount`, `proxy_ips`. Only the first `listen-socket` is used. |
| `authentication`: `source-password`, `admin-user`, `admin-password`, `health-password`                                     | the parameters of the same names                                                                             |
| `limits`: `sources`, `clients`, `client-timeout`, `source-timeout`, `burst-size`, `burst-on-connect`, `max-listeners` (KH) | `max_sources`, `max_clients`, the timeouts, the burst, and every mount's default `max_listeners`             |
| `paths`: `logdir`, `webroot`, certificate and key, `x-forwarded-for`, `allow-ip`, `deny-ip`, `deny-agents`                 | log locations, the preroll directory, the TLS transport, `proxy_ips`, access lists on every mount            |
| `logging`: `accesslog`, `errorlog`, `playlistlog`, `loglevel`, `logsize`, `logarchive`                                     | the log parameters; files go under `logdir`, and `logsize` is in KiB                                         |
| `http-headers`, and RSAS's `custom-headers`                                                                                | `http_headers`, on the server or the mount                                                                   |
| `event-bindings`                                                                                                           | `event_bindings`, running the `exec`, `url` and `log` actions                                                |
| `mount`                                                                                                                    | the `mount` callback (see below)                                                                             |

In a `<mount>`, `mount-name`, `username`, `password`, `max-listeners`, `burst-size`, `fallback-mount`, `fallback-when-full`, `linger-for` (KH), `hidden`, `on-connect`, `on-disconnect`, `http-headers`, `max-listener-duration`, `max-history`, `intro`, `preroll` (RSAS), `dump-file` and its limits, `health-password` and `authentication` are translated.

A path takes the `type="normal"` mount of that name, completed field by field with the first `type="default"` mount whose `mount-name` pattern matches, or that has none. Under `"kh"`, every `mount-name` is a pattern and the lowest `priority` wins. `<authentication type="url">` becomes URL authentication with the same options, including `mount_add`, `mount_remove` and `stream_auth`. `type="htpasswd"` becomes the htpasswd decision, and `allow_duplicate_users` set to `0` or `connections-per-user` limits each user's listeners. Access lists come first in every mount's decision, then authentication, then the `intro` and the `preroll`.

`fallback-override` is accepted and changes nothing: listeners always come back to the mount's own source. Directory listings (YP), relays and master/slave setups, `fileserve`, `alias` and `security` are not available and warn.

## A complete example

This server keeps an automated backup behind its live mount, protects a members mount with an htpasswd file, writes an access log, and reloads on `SIGHUP`:

```{.liquidsoap include="icecast-server-complete.liq"}

```

## See also

- [Harbor](./harbor.md): the input that receives streams, which `icecast.server` builds on.
- [Harbor HTTP](./harbor_http.md): serving your own HTTP endpoints, on the same port if you like.
- [HLS output](./hls_output.md): another way to deliver streams to listeners.
