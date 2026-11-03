# Observability: events, statistics, pages, metrics and logs

What the server reports to its operator and to the software around it:
events, the statistics snapshot, the status, admin, health and metrics pages,
and the log files. Page shapes and log formats are a compatibility surface:
players, widgets and log analysers parse them, so they are specified field by
field.

Source clients, mounts and listener admission are owned by
[sources.md](sources.md), [mounts.md](mounts.md) and
[listeners.md](listeners.md). Parameters, their types, defaults and read points
are owned by [parameters.md](parameters.md). Delivery of bytes, and the
listener session the events describe, are owned by the
[harbor output specification](../../../../core/outputs/spec/harbor/README.md).
Features that are not specified here are listed in
[future-work.md](future-work.md).

The `compatibility` parameter ([parameters.md](parameters.md)) chooses the
implementation mimicked (`"icecast-2.5"`, `"icecast-2.4"`, `"rsas"`, `"kh"`).
Rules that depend on it say what each value does. Rules without that mention
hold for every value.

Signals are not owned by the server. Reloading and reopening logs are methods
([§9](#9-log-files)); binding them to `SIGHUP` or another trigger is the
script's.

## 1. Events

The server reports events to handlers the operator registers through server
methods. Methods, not parameters: a reload replaces the parameters and MUST NOT
drop registered handlers.

| Method                   | When                                                                                     | Handler receives                                                                                                                                                                                                                                                                                                                      |
| ------------------------ | ---------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `on_source_connect`      | A source client or Liquidsoap source starts feeding a mount, and its mount output exists | the source-connect record: `mount`, `source` (the Liquidsoap source feeding the mount output), `format`, `headers` (the headers a source client sent, empty for a Liquidsoap source), `streams` (the description of each stream the harbor input found, codec and parameters, empty for a Liquidsoap source), `ip`, `agent`, `server` |
| `on_source_disconnect`   | What fed a mount is gone and the mount is updated ([mounts.md](mounts.md))               | the source-disconnect record: `mount`, `ip`, `agent`, `server`                                                                                                                                                                                                                                                                        |
| `on_listener_connect`    | A listener is admitted and its session starts                                            | the [listener record](#2-the-listener-record) with `duration` and `bytes_sent` zero                                                                                                                                                                                                                                                   |
| `on_listener_disconnect` | A listener session ends, for any reason                                                  | the listener record with final `duration` and `bytes_sent`                                                                                                                                                                                                                                                                            |
| `on_metadata`            | A mount's metadata changes ([mounts.md](mounts.md))                                      | `mount`, `metadata` (a list of key and value pairs), `listeners` (the mount's current listener count)                                                                                                                                                                                                                                 |

Each method takes `synchronous` and a callback, and may be called any number of
times. `synchronous` defaults to `false`. A mount's `on_connect` and
`on_disconnect` callbacks ([parameters.md](parameters.md#52-source-callbacks))
receive the source-connect and source-disconnect records.

**Source records.** `ip` is the source client's address, never hashed, and
`agent` its `User-Agent`, `-` when absent; both are empty for a Liquidsoap
source. The disconnect record carries the same two as the connect record of the
same feeder, so a notification on departure needs no state of its own. `server`
is `{hostname, port, instance}` ([parameters.md §5](parameters.md#5-callbacks)):
the server fills it, and a handler or a helper built outside the server uses it
as it is.

Icecast's `event-bindings` are the `event_bindings` parameter
([parameters.md §5.5](parameters.md#55-event-bindings)), not handlers
registered on these methods: a reload replaces them, and it does not touch the
handlers. Each binding is called with `{event, mount, listeners,
compatibility}` ([parameters.md §5.5](parameters.md#55-event-bindings)). Their
triggers map as follows; [icecast-config.md](icecast-config.md) owns the
translation of the `exec` and `url` actions.

| Icecast trigger                                                   | Server event                                                                        |
| ----------------------------------------------------------------- | ----------------------------------------------------------------------------------- |
| `source-connect`, `source-disconnect`                             | `on_source_connect`, `on_source_disconnect`                                         |
| `format-metadata-changed`                                         | `on_metadata`                                                                       |
| `source-listener-attach`, `source-listeners-changed`              | `on_listener_connect` and `on_listener_disconnect`, each with the mount's new count |
| `source-listeners-is-zero`                                        | `on_listener_disconnect` that leaves the mount with no listener                     |
| `dumpfile-opened`, `dumpfile-closed`, `dumpfile-error`            | Not implemented: the dump file ([mounts.md](mounts.md#9-dump-file)) raises no event |
| `icecast-start`, `icecast-stop`, `icecast-active`, `icecast-idle` | Not server events: the script uses Liquidsoap's own start and shutdown hooks        |

## 2. The listener record

One record describes a listener. Events, statistics, the admin pages and the
access log read it; nothing else defines it.

| Field          | Meaning                                                                                                                                   |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| `id`           | Connection identifier, unique within a mount output                                                                                       |
| `mount`        | Mount the listener reads                                                                                                                  |
| `ip`           | Resolved address ([listeners.md](listeners.md#8-the-listeners-address)) transformed by `address_hash` ([§12](#12-where-addresses-appear)) |
| `user`         | Authenticated user name, absent when none                                                                                                 |
| `user_agent`   | `User-Agent` request header, absent when not sent                                                                                         |
| `referer`      | `Referer` request header, absent when not sent                                                                                            |
| `uri`          | Requested path                                                                                                                            |
| `protocol`     | HTTP version, such as `1.1`                                                                                                               |
| `connected_at` | Connection time, seconds since the epoch (float)                                                                                          |
| `duration`     | Seconds connected (float): zero on connect, the session length on disconnect                                                              |
| `bytes_sent`   | Bytes written to the listener: the response head, the preroll and the stream. Zero on connect, the total on disconnect                    |

The record MUST NOT carry the raw request headers: they can hold the
listener's address (for instance `X-Forwarded-For`).

## 3. Ordering guarantees

- Every admitted listener produces exactly one `on_listener_connect` and
  exactly one `on_listener_disconnect`. A refused listener produces neither.
- A listener's disconnect handlers run after its connect handlers.
- Every source that connects produces exactly one `on_source_disconnect`, after
  its `on_source_connect`.
- A synchronous handler runs before the path that produced the event proceeds,
  in registration order. It MUST be short: the streaming path waits for it. For
  one listener, its synchronous connect handlers have all returned before any
  of its synchronous disconnect handlers start.
- An asynchronous handler cannot delay streaming. No order is guaranteed
  between asynchronous handlers, nor between an asynchronous handler and
  anything else. Concurrent asynchronous handlers MUST NOT be assumed to see
  state in event order, and counters they share MUST be atomic.
- A handler that raises is logged at the `severe` level. It MUST NOT affect
  streaming, other handlers or the session.
- The statistics of [§4](#4-statistics) include an event before any
  synchronous handler for it runs, so `stats()` called from a synchronous
  handler already counts it. The access and playlist log lines
  ([§10](#10-access-log), [§11](#11-playlist-log)) are written once per event
  whatever the handlers do.

Rationale: counters must never drift, while operator code (databases, HTTP
calls) must never stall the stream.

## 4. Statistics

The server keeps, for every mount output:

| Statistic        | Meaning                                                                                                                                                                                                                                                                  |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `listeners`      | The listener records of the listeners connected now                                                                                                                                                                                                                      |
| `peak_listeners` | The largest number of simultaneous listeners since the mount output was created                                                                                                                                                                                          |
| `connections`    | The number of listener connections admitted since the mount output was created                                                                                                                                                                                           |
| `listening_time` | Sum of `duration` over finished sessions, in seconds                                                                                                                                                                                                                     |
| `bytes_sent`     | Sum of `bytes_sent` over finished sessions                                                                                                                                                                                                                               |
| `bytes_read`     | Bytes read so far from the mount's current source client; 0 without one                                                                                                                                                                                                  |
| `metadata`       | The mount's latest metadata, empty before any                                                                                                                                                                                                                            |
| `history`        | The last titles played, newest first ([§13](#13-metadata-history))                                                                                                                                                                                                       |
| stream identity  | `name`, `description`, `genre`, `url` (from the source client's stream headers, [sources.md](sources.md#5-stream-identity-and-content); empty when absent), `public`, `content_type`, `mime_type`, and `bitrate`, `channels`, `samplerate` when the source declared them |
| `started`        | When the mount's current source connected, seconds since the epoch                                                                                                                                                                                                       |
| `fallback`       | The mount's effective fallback, run-time overlay included: its path, or `(source)` for a Liquidsoap source; absent when none                                                                                                                                             |
| `hidden`         | Whether the mount is hidden ([mounts.md](mounts.md))                                                                                                                                                                                                                     |

Rules:

- `peak_listeners` never decreases and is at least the current count.
- `connections` counts at connect; `listening_time` and `bytes_sent` change
  only at disconnect. Live sessions are not in the totals.
- `started`, `bytes_read` and the stream identity describe the current source:
  a source that reconnects replaces them. Counters persist across source reconnections for as
  long as the mount output exists ([mounts.md](mounts.md)).

The server also keeps `server_start` (seconds since the epoch),
`last_reload` (the last `reload`, `server_start` before any), and the counts
`listener_connections` and `source_connections` admitted since start. The
number of listeners connected is the sum over mounts and enforces `max_clients`
([listeners.md](listeners.md)).

## 5. The `stats()` snapshot

`stats()` is a server method returning one record: the server-wide values of
[§4](#4-statistics) and, for every mount that has an output, its statistics.
Every page and the metrics export are renderings of it.

- Taking one MUST NOT block streaming. It MAY be slightly stale relative to
  events in flight, but each mount's listener list and counters are read
  together.
- It includes hidden mounts and the listener records. A page that is public
  filters them ([§6](#6-the-status-page)).

## 6. The status page

When `status_page` is on, the server serves these paths as HTTP handlers on its
own port, with the other handlers of the port. A route the script registers on
that port before calling `icecast.server` takes precedence over the server's;
a `GET` route registered after it is shadowed by the server's handlers.

| Path               | Content                                                                                         |
| ------------------ | ----------------------------------------------------------------------------------------------- |
| `/`                | HTML page: per mount, name, format, start time, now playing, listener count, peak, and a player |
| `/status.json`     | JSON object `{"mounts": [...]}`, one entry per mount                                            |
| `/status-json.xsl` | JSON in Icecast's shape ([§6.1](#61-status-jsonxsl))                                            |

A `/status.json` mount entry has `mount`, `name`, `content_type`, `mime_type`,
`started`, `listener_count`, `peak_listeners` and `current_metadata`, a list of
`{"key", "value"}` pairs.

- When `status_page` is off, none of the paths is served (they answer as any
  unknown path, [mounts.md](mounts.md)). The admin pages
  ([§7](#7-the-admin-pages)) and `/health` are not affected by the switch of
  the status page, except as stated in [§7.4](#74-health).
- `status_access` is a callback taking the HTTP request and returning whether
  to allow it. It is the ordinary access control of a handler: a refusal is a
  `401` with `WWW-Authenticate: Basic realm="Icecast2 Server"` and an empty
  body. Unset, every request is allowed. Rationale: that is Icecast's realm;
  clients that cache credentials key them by realm. A callback that raises
  refuses the request with that `401` and the error is logged.
- `status_json` and `status_html`, when set, are callbacks that replace the
  built-in body of `/status.json` and `/`. Each receives the `stats()` snapshot
  ([§5](#5-the-stats-snapshot)) and returns the body. A callback that fails
  yields a `500` and MUST NOT affect streaming. `/status-json.xsl` is not
  replaceable: its shape is fixed by compatibility.
- The pages MUST NOT show listener addresses, user agents, referrers, source
  passwords or addresses, or any parameter. They show counts only.
- Mounts that are hidden MUST NOT appear on any of the three paths.
  Rationale: Icecast keeps them off its public pages.
- Values from clients and source clients (names, metadata) MUST be inserted into
  HTML as text, never as markup, and into JSON as escaped strings.

Departure from Icecast: `/` and `/status.json` are this server's, where
mainline serves `/status.xsl`. The XSL pages and the XML stats of mainline are
not provided at these paths.

### 6.1 `/status-json.xsl`

Players and widgets read this path, so its shape MUST be Icecast's:

```
{"icestats": {"admin": "...", "host": "...", "location": "...",
              "server_id": "...", "server_start": "...",
              "server_start_iso8601": "...", "source": {...}}}
```

- `source` is absent when no mount is listed, a single object when one is, and
  an array of objects when several are, in mount-name order. Rationale: that is
  what Icecast's transformation yields and clients rely on it.
- Top-level `admin` is `admin_contact`, `host` is `hostname`, `location` is
  `location`, `server_id` is `server_id` ([parameters.md](parameters.md)).
  `server_start` is the start time as `%a, %d %b %Y %H:%M:%S %z`
  and `server_start_iso8601` as `%Y-%m-%dT%H:%M:%S%z`, both in server local
  time.
- Each `source` object has `listenurl`, `listeners`, `listener_peak`,
  `server_name`, `server_description`, `server_type`, `server_url`, `genre`,
  `stream_start`, `stream_start_iso8601` (formatted as above, for `started`),
  and, when known, `title`, `bitrate`, `channels`, `samplerate` and
  `audio_info` (`ice-samplerate=44100;ice-bitrate=128;ice-channels=2`).
  `listeners`, `listener_peak`, `bitrate`, `channels` and `samplerate` are JSON
  numbers; the others strings. The mount path is `listenurl`'s path.
- `listenurl` is the scheme (`https` when the `transport` is TLS), the request's `Host`
  header, or `hostname` and `port` without one, and the mount.
- `title` is the current text as composed in [§11](#11-playlist-log) (`artist -
title`). Mainline gives Ogg mounts separate `artist` and `title`; this server
  composes for every container so that one reader works on all.
- Hidden mounts, listener addresses, per-source totals, source addresses and
  source user agents are not part of it (Icecast removes them as well).
- Content type `application/json`.

## 7. The admin pages

The admin pages are HTTP handlers on the server's port. They are not governed
by `status_page`.

### 7.1 Access

- Who may issue an admin request, and the `401` answers (`admin_password`
  unset included), are [sources.md §6](sources.md#6-admin-requests)'s.
  `status_access` does not apply to these pages. `/admin/listclients` is a
  mount command there.
- `admin_user` and `admin_password` are read at each request, so a reload
  applies at once.
- Listener addresses on these pages are transformed by `address_hash` like
  everywhere ([§12](#12-where-addresses-appear)). Rationale: the operator
  opts out of privacy once, in `address_hash`, not per page.
- Client-controlled fields MUST be inserted as text (HTML) or escaped (XML,
  JSON).

### 7.2 Liquidsoap pages

| Path                    | Content                                                                                                                                                                     |
| ----------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `/admin/listeners`      | HTML page, refreshed periodically: per mount, listeners, peak, connections, listening time, and a table of listeners                                                        |
| `/admin/listeners.json` | JSON `{"mounts": [...]}`: per mount, `mount`, `peak_listeners`, `connections`, `listening_time`, `bytes_sent`, `listeners` (the [listener records](#2-the-listener-record)) |

### 7.3 Icecast XML

Three admin paths return Icecast's XML, because log analysers and
administration tools read them. All answer `200`, `Content-Type: text/xml`,
with an `<icestats>` root. Hidden mounts are included. Sub-elements are
produced from the snapshot; values are text, numbers decimal.

**`/admin/stats`** (all `compatibility` values). Children of `<icestats>`:

- `admin`, `host`, `location`, `server_id`, `server_start`,
  `server_start_iso8601` as in [§6.1](#61-status-jsonxsl);
- `sources` (mounts with a source), `listeners` (connected now), `clients`
  (listeners and sources), `listener_connections` and
  `source_client_connections` (admitted since start), `client_connections` and
  `connections` (their sum), `source_total_connections` (equal to
  `source_client_connections`);
- `file_connections`, `stats`, `stats_connections`, `banned_IPs`: `0`. Tools
  read them and the server has no such count.
- one `<source mount="/path">` per mount, with `listenurl`, `listeners`,
  `listener_peak`, `server_name`, `server_description`, `server_type`,
  `server_url`, `genre`, `stream_start`, `stream_start_iso8601`, `title` and
  the audio keys of [§6.1](#61-status-jsonxsl) (plus `ice-bitrate`),
  `max_listeners` (the mount's `max_listeners`, or `unlimited`), `public`
  (`0` or `1`), `slow_listeners` (`0`), `total_bytes_sent` (`bytes_sent`
  including live sessions), `total_bytes_read` (`bytes_read`), `connected` (seconds since `started`), `source_ip` (the
  source client's raw address; absent for a Liquidsoap source) and
  `user_agent` (the source client's).

**`/admin/listmounts`**. One `<source mount="/path">` per mount that has an
output, with `listeners`, `Listeners` (the same number: tools of both
generations read one of them), `connected`, `content-type`, and `fallback` when
a fallback is configured: the fallback mount's path, or `(source)` for a
Liquidsoap source (no mount path starts with `(`, so it cannot be mistaken
for one).

**`/admin/listclients?mount=<mount>`**. One `<source mount="/path">` with
`Listeners` (the count) and one `<listener>` per listener with `ID`, `IP`,
`UserAgent`, `Connected` (seconds) and, when known, `referer` and `username`.
Names use the capitalisation of Icecast's and RSAS's responses. A missing
`mount` is `400`; an unknown mount follows the table of
[sources.md §6](sources.md#6-admin-requests).

Mainline additionally serves these under `.xsl` and `.json` names; they are not
provided.

### 7.4 `/health`

`/health` is the JSON document RSAS documents, served for every
`compatibility` value (a behaviour only RSAS has, conflicting with nothing):

```
{"last_config_reload": 1630092595,
 "mounts": {"/main": {"format": {"bitrate": 128, "channels": 2,
                                 "content_type": "audio/mpeg",
                                 "codec": "MP3", "samplerate": 44100},
                      "listener_count": 37,
                      "metadata": {"now_playing": "Artist - Title"}}},
 "total_listener_count": 37, "total_source_count": 1}
```

- `last_config_reload` is `last_reload` rounded to whole seconds.
  `total_source_count` is the mounts that have a source; every mount with an
  output is in `mounts`, hidden ones excepted. `format` keys are present when
  known; `codec` is the codec's common name. `now_playing` is the text of
  [§11](#11-playlist-log), absent while the mount has none.
- `/<mount>/health` returns that mount's entry. A hidden or unknown mount is
  `404`, answered before any password is checked. Rationale: a challenge would
  tell a prober that the hidden mount exists. A mount whose name ends in
  `/health` cannot be served: the path is reserved ([sources.md](sources.md)).
- `health_password`, global or in the mount's parameters (the mount's own,
  else the global one), protects the matching path with HTTP Basic: the
  password is checked, the user name is not. Unset, the path is open. A refusal
  is a `401` with the realm of [§6](#6-the-status-page).
- `/health` is served when `status_page` is on, or `compatibility` is `"rsas"`.
  Rationale: RSAS provides it independently of its status page emulation;
  the other servers have nothing like it, so it follows the page switch.

## 8. Metrics

When `metrics` is on (default off), the server serves `/metrics` in the
Prometheus text exposition format (`Content-Type: text/plain; version=0.0.4;
charset=utf-8`), rendered from the snapshot. It follows `status_access`
([§6](#6-the-status-page)).

| Metric                                      | Type    | Labels  | Value                                 |
| ------------------------------------------- | ------- | ------- | ------------------------------------- |
| `icecast_server_start_time_seconds`         | gauge   |         | `server_start`                        |
| `icecast_server_last_reload_time_seconds`   | gauge   |         | `last_reload`                         |
| `icecast_server_mounts`                     | gauge   |         | mounts listed below                   |
| `icecast_server_source_up`                  | gauge   | `mount` | 1 when the mount has a source, else 0 |
| `icecast_server_source_start_time_seconds`  | gauge   | `mount` | `started`                             |
| `icecast_server_listeners`                  | gauge   | `mount` | current listeners                     |
| `icecast_server_listeners_peak`             | gauge   | `mount` | `peak_listeners`                      |
| `icecast_server_listener_connections_total` | counter | `mount` | `connections`                         |
| `icecast_server_listening_seconds_total`    | counter | `mount` | `listening_time`                      |
| `icecast_server_sent_bytes_total`           | counter | `mount` | `bytes_sent`                          |

- Hidden mounts are omitted. No label or value carries a listener address, user
  agent, referer or metadata.
- Label values are escaped as the format requires (`\\`, `\"`, `\n`).
- Counts are written as integers; times (`server_start`, `last_reload`,
  `started`, `listening_time`) with three decimals.
- Counters restart at zero when a mount output is removed and created again;
  scrapers treat that as a counter reset.

## 9. Log files

Three files exist: access, playlist and error log, with the targets
`access_log`, `playlist_log` and `error_log`.

- A target is a file path, `-` (standard error), or unset (the file log is
  disabled). `-` is never rotated.
- Every line ends with a single `\n`. Files are opened for append and created
  when absent.
- Concurrent writers MUST NOT interleave bytes within a line.
- A write failure MUST NOT affect streaming; it is logged at the `severe`
  level.
- When a target changes (reload), later lines go to the new target.

Rotation:

- `log_size` is the size in bytes past which a file rotates. The size counts
  the bytes already in the file at open plus the bytes written.
- Before writing a line, if the size is strictly greater than the limit, the
  file is closed, renamed, and a new file opened at the original path.
- Name: `<file>.old` (replacing a previous `.old`) when `log_archive` is off;
  `<file>.YYYYmmdd_HHMMSS` in local time when on.

`reopen_logs()` is a server method: all log files are closed and reopened at
their configured paths, so an external rotator can move them. Truncation by an
external tool needs no call. The server does not bind it to a signal.

Rationale: Icecast's own rotation and its packaging's `logrotate` rules rely on
these names and on a reopen call.

## 10. Access log

One line is written when a listener session ends, stamped with the **end**
time. Only admitted sessions are logged: refused requests, source connections
and page requests are not. Departure from Icecast (which logs every finished
request with its status): a refusal is visible to the operator through the
error log and the callbacks, and a refused request has no session.

```
<ip> - <user> [<date>] "GET <uri> HTTP/<protocol>" 200 <bytes> "<referer>" "<user-agent>" <duration>
```

| Field          | Content                                                                                                                     |
| -------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `<ip>`         | The record's `ip`, escaped                                                                                                  |
| identity       | Always `-`                                                                                                                  |
| `<user>`       | The record's `user`, else `-`                                                                                               |
| `<date>`       | `%d/%b/%Y:%H:%M:%S %z`: day, English month abbreviation, year, 24-hour time, numeric zone, server local time                |
| request        | `GET`, the `uri` (the path, without the query string, as Icecast logs it: queries carry tokens), `HTTP/` and the `protocol` |
| status         | `200`                                                                                                                       |
| `<bytes>`      | `bytes_sent`, decimal                                                                                                       |
| `<referer>`    | `referer`, else `-`                                                                                                         |
| `<user-agent>` | `user_agent`, else `-`                                                                                                      |
| `<duration>`   | Whole seconds connected, truncated                                                                                          |

Escaping depends on `compatibility`:

| Value                                      | Rule                                                                                                                                                                                                                                         |
| ------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `"icecast-2.5"`, `"icecast-2.4"`, `"rsas"` | Each byte that is a control character, space, `!`, `"`, `` ` `` or `\` is written as `\xHH` (two lowercase hex digits). In the referer and user agent, spaces are kept. Bytes from `0x80` up are written unchanged, so UTF-8 stays readable. |
| `"kh"`                                     | Request, referer and user agent are written as received, without escaping, the referer and the user agent cut to their first 150 bytes. The user name is percent-encoded (`%HH`), `-` when empty.                                            |

Rationale: analysers match each implementation's form. RSAS documents its
format as Icecast's without describing the escaping, so under `"rsas"` the
Icecast rule is this server's. Mainline escapes bytes from `0x80` up on platforms where `char` is signed; this
server keeps them readable on all platforms.

A request, referer or user agent containing a line break MUST NOT split the
line: under `"kh"`, line breaks are replaced by spaces.

Worked example (`"icecast-2.5"`, default `address_hash`):

```
8c2f0e7a1d4b9f36 - - [14/Sep/2026:21:40:58 +0200] "GET /live HTTP/1.1" 200 1843200 "-" "VLC/3.0.20 LibVLC/3.0.20" 115
```

A session of 115 s on `/live`, 1 843 200 bytes sent, no referer, a user agent
containing a space (kept).

## 11. Playlist log

See [§9](#9-log-files) for files. One line per metadata update that has text,
in every `compatibility` value (Icecast, RSAS and KH agree):

```
<date>|<mount>|<listeners>|<text>
```

- `<date>` is as in [§10](#10-access-log), the time of the update.
- `<mount>` is the mount path; `<listeners>` the mount's current listener count,
  decimal.
- `<text>` is `artist - title` when both are non-empty, else the one that is
  non-empty. An update with neither writes no line. This text is also the
  `now_playing` and `title` of the pages.
- Every `|`, carriage return and line feed in `<text>` is replaced by one space.
  Departure from Icecast, which writes the text unchanged: here one update is
  one line with four fields, so analysers do not misparse it. RSAS escapes `|`
  in its playlist log too, so under `"rsas"` this is compatibility. This is not
  subject to the other values.

Example: `14/Sep/2026:21:40:58 +0200|/live|12|Artist - Title`.

## 12. Where addresses appear

Listener addresses are personal data. This section owns where an address
appears, raw or hashed; the resolution (proxy list) and the hashing and opting
out are [listeners.md §8 and §9](listeners.md#8-the-listeners-address).

| Place                                                                   | Address                                                                     |
| ----------------------------------------------------------------------- | --------------------------------------------------------------------------- |
| Listener record (every event), `stats()`, pages, access log             | Resolved address transformed by `address_hash`                              |
| URL authentication requests to the backend ([url-auth.md](url-auth.md)) | Raw                                                                         |
| Source records and notifications ([§1](#1-events))                      | Raw: they describe the operator's own source clients                        |
| Error log, and the server's own log                                     | May contain the raw `ip:port` of a connection (as Icecast's error log does) |

With the default `address_hash`, `ip` is a fixed-length hexadecimal string, not
an address: analysers that validate it need that validation disabled, or
`address_hash` set to the identity.

## 13. Metadata history

`max_history` ([parameters.md](parameters.md#41-fields): server-wide, a mount's
own value wins) is the number of titles the mount keeps. Each `on_metadata`
with text adds the text
and its time to the mount's `history`, newest first; the oldest goes when the
list is full. It is part of the snapshot only. Rationale: mainline exposes it as an XSPF
document in its XML stats, a format this server does not provide. The parameter
applies under every `compatibility` value.

## 14. The server log and the error log file

The server logs through Liquidsoap's logger, with a label of its own, at
Liquidsoap's levels: 1 critical, 2 severe, 3 important, 4 info, 5 debug.
Whatever the operator configures for Liquidsoap's log applies to it.

The Icecast-format error log is an additional sink for the same messages,
written to `error_log` when set. `log_level` (Liquidsoap scale, default 4) is
the most verbose level written there. Lower levels are always written.

```
[<date>] <LEVEL> <category> <message>
```

- `<date>` is `%Y-%m-%d  %H:%M:%S` in brackets, local time, with **two** spaces
  between date and time, as Icecast writes it.
- `<LEVEL>` follows the level of the message:

  | Liquidsoap level     | `<LEVEL>` | Icecast level |
  | -------------------- | --------- | ------------- |
  | 1 critical, 2 severe | `EROR`    | 1             |
  | 3 important          | `WARN`    | 2             |
  | 4 info               | `INFO`    | 3             |
  | 5 debug              | `DBUG`    | 4             |

  Icecast level `n` of a configuration file is Liquidsoap level `n + 1`
  ([icecast-config.md](icecast-config.md) converts it). Rationale: Icecast's
  default, `INFO`, then keeps its meaning of "normal operation" and
  Liquidsoap's default level 3 hides it.

- `<category>` is `module/function`, reusing Icecast's where an equivalent event
  exists, so existing parsers keep matching.
- Values that come from clients (mount names, headers) in a message are
  escaped as in [§10](#10-access-log) for `"icecast-2.5"`,
  `"icecast-2.4"` and `"rsas"`. This binds the messages that have an Icecast
  category; the others use the category `icecast/server`.
- Messages logged where no server is at hand (parameter validation) and the
  failures of the log files themselves go to Liquidsoap's log only.

Binding examples, which parsers match:

```
[2026-09-25  15:17:57] INFO connection-handle/_handle_source_request Source logging in at mountpoint "/live"
[2026-09-25  15:20:01] INFO source/source_main Source at "/live" exiting
```

Message wording beyond these is incidental.

## 15. Conformance

**Binding**

1. Events. Per admitted listener, exactly one connect and one later
   disconnect, with `duration` and `bytes_sent` zero then final; none for a
   refused one. Same for sources. Synchronous handlers precede the next stage
   of the producing path and run in registration order; an asynchronous handler
   that blocks does not delay streaming or other listeners; a raising handler
   changes nothing else. A reload keeps the handlers. `bytes_sent` includes the
   response head and the preroll.
2. Statistics. After a known series of sessions, peak, connections, listening
   time, bytes sent and current listeners match; live sessions are not in the
   totals; counters survive a source reconnect and `started` changes with it;
   `history` is bounded by `max_history`.
3. Status pages. `/`, `/status.json` and `/status-json.xsl` exist exactly when
   `status_page` is on; a refusal by `status_access` is `401` with the realm
   header; callbacks replace the bodies of `/` and `/status.json` only; hidden
   mounts and addresses never appear. `/status-json.xsl` parses to
   `{"icestats": ...}`, with `source` absent, an object, or an array according
   to the number of mounts, numbers as numbers.
4. Admin and health. The admin pages answer `401` without `admin_password` or
   without credentials, `200` with the admin's, whatever `status_page` says; a
   mount command (`/admin/listclients`) answers with the mount's source
   credentials even without `admin_password`; they show hashed addresses and
   hidden mounts. `/admin/stats`, `/admin/listmounts` and
   `/admin/listclients` have the elements of [§7.3](#73-icecast-xml).
   `/health` has the keys of [§7.4](#74-health) and honours `health_password`.
5. Metrics. `/metrics` exists exactly when `metrics` is on, with the metrics
   above, without hidden mounts or personal data.
6. Access log. Exactly one line per finished admitted session; field by field
   byte equality against the format for each `compatibility` value, including
   the escape of each special byte, spaces kept in referer and user agent,
   bytes `0x80` and up unchanged (non-`"kh"`), `-` for absent fields, truncated
   whole-second duration, the date with zone, one line even with line breaks
   in headers.
7. Playlist log. `artist - title` composition, no line without text, `|`, CR and
   LF replaced by a space, listener count correct.
8. Error log. The timestamp with two spaces, the level names of the mapping,
   filtering by `log_level`, the two example lines.
9. Files. `-` writes to standard error and never rotates; a file rotates at the
   next write after it exceeds `log_size`; `.old` is replaced and archive names
   match `YYYYmmdd_HHMMSS`; `reopen_logs()` reopens at the configured path; a
   changed target takes effect for the next line; concurrent writes never
   interleave within a line.
10. Privacy. With the default `address_hash` no raw listener address is found in
    events, `stats()`, pages, metrics or the access log.

**Incidental**: HTML markup, styling, refresh interval, the order of elements
inside an XML object, message wording of logs, relative timing of asynchronous
handlers, the actual times.

**Doubles**: a fake clock for dates and durations; a fixed `address_hash`; log
targets in a temporary directory.

**Assert something ran**: a count of events received, of lines written and of
pages fetched, checked before the suite reports a pass.
