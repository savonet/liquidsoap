# Observability: events, statistics, pages and logs

What the server reports to its operator: events, statistics, the status and
admin pages, and the log files. Log formats are a compatibility surface: log
analysers parse them, so they are specified byte for byte.

Sources, mounts and listener admission are owned by
[sources.md](sources.md), [mounts.md](mounts.md) and
[listeners.md](listeners.md). Settings are owned by [settings.md](settings.md).
Delivery of bytes, and the listener session the events describe, are owned by
the [harbor output specification](../../../../core/outputs/spec/harbor/README.md).

The `compatibility` setting ([settings.md](settings.md)) chooses the
implementation mimicked (`icecast-2.5`, `icecast-2.4`, `rsas`, `kh`). Rules
that depend on it say what each value does. Rules without that mention hold
for every value.

## 1. Events

The server reports five kinds of event to handlers the operator registers.

| Event               | When                                                                    | Payload                                                           |
| ------------------- | ----------------------------------------------------------------------- | ----------------------------------------------------------------- |
| source connect      | A source is accepted on a mount and its mount output exists             | mount, stream format, streams, the headers the source sent        |
| source disconnect   | A source that connected has gone, its mount updated                     | mount                                                             |
| listener connect    | A listener is admitted and its session starts                           | [listener record](#2-the-listener-record), `duration` and `bytes_sent` zero |
| listener disconnect | A listener session ends, for any reason                                 | the listener record with final `duration` and `bytes_sent`        |
| metadata update     | A mount's metadata changes ([mounts.md](mounts.md))                     | mount, the new metadata, the mount's current listener count       |

Each kind has any number of handlers. A handler is **synchronous** or
**asynchronous**, chosen by the operator at registration.

## 2. The listener record

| Field          | Meaning                                                                                                    |
| -------------- | ---------------------------------------------------------------------------------------------------------- |
| `id`           | Connection identifier, unique within a mount output                                                        |
| `mount`        | Mount the listener reads                                                                                   |
| `ip`           | Listener address after proxy resolution, then transformed by `address_hash` ([§11](#11-where-addresses-appear)) |
| `user_agent`   | `User-Agent` request header, absent when not sent                                                          |
| `referer`      | `Referer` request header, absent when not sent                                                             |
| `uri`          | Requested path                                                                                             |
| `protocol`     | HTTP version, such as `1.1`                                                                                |
| `connected_at` | Connection time, seconds since the epoch                                                                   |
| `duration`     | Seconds connected: zero on connect, the session length on disconnect                                       |
| `bytes_sent`   | Bytes written to the listener, response head included: zero on connect, the total on disconnect            |

The record MUST NOT carry the raw request headers: they can hold the
listener's address (for instance `X-Forwarded-For`).

## 3. Ordering guarantees

- Every admitted listener produces exactly one listener connect and exactly
  one listener disconnect. A refused listener produces neither.
- A listener's disconnect handlers run after its connect handlers.
- Every source that connects produces exactly one source disconnect, after its
  source connect.
- Synchronous handlers run in the path that produced the event, in registration
  order, before it proceeds. They MUST be short: the streaming path waits for
  them. For one listener, its synchronous connect handlers have all returned
  before any of its synchronous disconnect handlers start.
- Asynchronous handlers each run in their own thread, so they cannot delay
  streaming. No order is guaranteed between asynchronous handlers, nor between
  an asynchronous handler and anything else. Concurrent asynchronous handlers
  MUST NOT be assumed to see state in event order.
- The server's own bookkeeping (counters of [§4](#4-statistics), the access log
  line, the playlist log line) is done by synchronous handlers, so a stats
  snapshot taken by a synchronous handler already includes the event.

Rationale: counters must never drift, while operator code (databases, HTTP
calls) must never stall the stream.

## 4. Statistics

The server keeps, for every mount output:

| Statistic          | Meaning                                                                                            |
| ------------------ | -------------------------------------------------------------------------------------------------- |
| listeners          | The listeners connected now (their records)                                                        |
| peak listeners     | The largest number of simultaneous listeners since the mount output was created                    |
| connections        | The number of listener connections admitted since the mount output was created                     |
| listening time     | Sum of `duration` over finished sessions, in seconds                                               |
| bytes sent         | Sum of `bytes_sent` over finished sessions                                                         |
| current metadata   | The mount's latest metadata, empty before any                                                      |
| stream identity    | `name` (the source's stream name, empty when absent), `content_type` (stream format), `mime_type`  |
| started            | When the mount's current source connected, seconds since the epoch                                 |

Rules:

- `peak listeners` never decreases and is at least the current count.
- `connections` counts at connect; `listening time` and `bytes sent` change
  only at disconnect. Live sessions are not in the totals.
- `started` and the stream identity describe the current source: a source that
  reconnects replaces them. Counters persist across source reconnections for as
  long as the mount output exists ([mounts.md](mounts.md)).
- Server-wide, the server counts the listeners connected on all mounts. That
  count enforces `max_clients` ([listeners.md](listeners.md)); it equals the sum
  of the mounts' current listeners and is not reported separately.

## 5. The stats snapshot

A snapshot lists every mount that has an output, with the statistics above.
Taking one MUST NOT block streaming. It MAY be slightly stale relative to events
in flight, but each mount's listener list and counters are read together.
Snapshots include hidden mounts (`hidden`, [mounts.md](mounts.md)); only the
public pages filter them ([§6](#6-the-status-page)).

## 6. The status page

When `status_page` is on, the server serves on its port:

| Path           | Content                                                             |
| -------------- | ------------------------------------------------------------------- |
| `/`            | HTML page: per mount, name, format, start time, now playing (artist and title), listener count, peak, and a player |
| `/status.json` | JSON object `{"mounts": [...]}`, one entry per mount                |

A JSON mount entry has `mount`, `name`, `content_type`, `mime_type`, `started`,
`listener_count`, `peak_listeners` and `current_metadata`, a list of
`{"key", "value"}` pairs.

- When `status_page` is off, neither path is served (they answer as any unknown
  path, [mounts.md](mounts.md)).
- `status_access` decides each request to either path. When it refuses, the
  server answers `401` with `WWW-Authenticate: Basic realm="Liquidsoap"` and an
  empty body. Absent, every request is allowed.
- `status_json` and `status_html`, when set, replace the built-in rendering of
  the respective path: each receives the stats snapshot ([§5](#5-the-stats-snapshot))
  and returns the body. The JSON path then answers `Content-Type:
  application/json` with the body as returned. A renderer that fails MUST yield
  a `500` and MUST NOT affect streaming.
- The pages MUST NOT show: listener addresses, user agents, referers, source
  passwords or addresses, or any setting. They show counts only.
- Mounts with `hidden` set MUST NOT appear in either path. Rationale: Icecast
  keeps them off its public pages.
- Values from clients or sources (names, metadata) MUST be inserted into the
  HTML as text, never as markup.

Departure from Icecast: the paths are `/` and `/status.json`, not mainline's
`/status.xsl` and `/status-json.xsl`. For `icecast-2.4`, `icecast-2.5`, `rsas`
and `kh`, the server SHOULD also answer `/status-json.xsl` with the same JSON
document; the XSL pages and the XML stats of mainline are not provided.

## 7. The admin listener page

For operators, the server serves, when `status_page` is on:

| Path                    | Content                                                                              |
| ----------------------- | ------------------------------------------------------------------------------------ |
| `/admin/listeners`      | HTML page, refreshed periodically: per mount, listeners, peak, connections, listening time, and a table of listeners |
| `/admin/listeners.json` | JSON `{"mounts": [...]}`: per mount, `mount`, `peak_listeners`, `connections`, `listening_time`, `bytes_sent`, `listeners` (the [listener records](#2-the-listener-record)) |

- Access needs HTTP Basic authentication with `admin_user` (default `admin`)
  and `admin_password`. A request without matching credentials gets `401` with
  `WWW-Authenticate: Basic realm="Liquidsoap"`.
- While `admin_password` is unset, both paths MUST answer as an unknown path
  (`404`), whatever the credentials. The server logs once at startup that the
  page is disabled. `status_access` does not apply: the admin credentials are
  the only gate.
- `admin_user` and `admin_password` are read at each request, so a reload
  applies at once.
- The listener `ip` is transformed by `address_hash` like everywhere
  ([§11](#11-where-addresses-appear)).
- Client-controlled fields MUST be inserted as text.

## 8. Log files

Three logs exist: access, playlist, error. Each has a target in `access_log`,
`playlist_log`, `error_log`.

- A target is a file path, `-` (standard error), or unset (log disabled).
  `-` is never a file and is never rotated.
- Every line ends with a single `\n`. Files are opened for append and created
  when absent.
- The log is a stream of independent lines: concurrent writers MUST NOT
  interleave bytes within a line.
- A write failure MUST NOT affect streaming; it is reported in the
  server's own diagnostics.
- When a target changes (reload), later lines go to the new target.

Rotation:

- `log_size` is the size in KiB past which a file rotates; default 1 000 000 000
  bytes when unset. The size counts the bytes already in the file at open plus
  the bytes written.
- Before writing a line, if the size is strictly greater than the limit, the
  file is closed, renamed, and a new file is opened at the original path.
- Name: `<file>.old` (replacing a previous `.old`) when `log_archive` is off;
  `<file>.YYYYmmdd_HHMMSS` in local time when on.
- On `SIGHUP` or `SIGUSR1` all log files are closed and reopened at their
  configured paths, so an external rotator can move them. Truncation by an
  external tool needs no signal.

Rationale: Icecast's own rotation and its packaging's `logrotate` rules rely on
exactly these names and signals.

## 9. Access log

One line is written when a listener session ends, stamped with the **end** time.
Only admitted sessions are logged: refused requests, source connections and
requests for the pages are not. Departure from Icecast (which logs every
finished request with its status): refusals are already visible to the operator
through the error log and the policy functions; logging them would require
sessions that never existed.

```
<ip> - <user> [<date>] "GET <uri> HTTP/<protocol>" 200 <bytes> "<referer>" "<user-agent>" <duration>
```

| Field        | Content                                                                                                  |
| ------------ | -------------------------------------------------------------------------------------------------------- |
| `<ip>`       | The listener's `ip` ([§11](#11-where-addresses-appear)), escaped                                          |
| identity     | Always `-`                                                                                                |
| `<user>`     | The authenticated listener's user name when known, else `-`                                               |
| `<date>`     | `%d/%b/%Y:%H:%M:%S %z`: day, English month abbreviation, year, 24-hour time, numeric zone, server local time |
| request      | `GET`, the `uri`, `HTTP/` and the `protocol`                                                              |
| status       | `200`                                                                                                     |
| `<bytes>`    | `bytes_sent`, decimal                                                                                     |
| `<referer>`  | `referer`, else `-`                                                                                       |
| `<user-agent>` | `user_agent`, else `-`                                                                                  |
| `<duration>` | Whole seconds connected, truncated                                                                        |

Escaping depends on `compatibility`:

| Value                     | Rule                                                                                                          |
| ------------------------- | ------------------------------------------------------------------------------------------------------------- |
| `icecast-2.5`, `icecast-2.4`, `rsas` | Each byte that is a control character, space, `!`, `"`, `` ` `` or `\` is written as `\xHH` (two lowercase hex digits). In the referer and user agent, spaces are kept. Bytes from `0x80` up are written unchanged, so UTF-8 stays readable. |
| `kh`                      | Request, referer and user agent are written as received, without escaping, the referer and the user agent cut to their first 150 bytes. The user name is percent-encoded (`%HH`), `-` when empty. |

Rationale: this is what each implementation writes; analysers match its form.
Mainline escapes bytes from `0x80` up on platforms where `char` is signed;
this server keeps them readable on all platforms. The `rsas` form is that of
mainline's combined format, which RSAS documents; it is not verified against
RSAS output.

A request, referer or user agent containing a line break MUST NOT split the
line: under `kh`, line breaks are replaced by spaces.

Worked example (`icecast-2.5`, default `address_hash`):

```
8c2f0e7a1d4b9f36 - - [14/Sep/2026:21:40:58 +0200] "GET /live HTTP/1.1" 200 1843200 "-" "VLC/3.0.20 LibVLC/3.0.20" 115
```

A session of 115 s on `/live`, 1 843 200 bytes sent, no referer, a user agent
containing a space (kept).

## 10. Playlist log

One line per metadata update that has text, in every `compatibility` value
(Icecast, RSAS and KH agree):

```
<date>|<mount>|<listeners>|<text>
```

- `<date>` is as in [§9](#9-access-log), the time of the update.
- `<mount>` is the mount path; `<listeners>` the mount's current listener count,
  decimal.
- `<text>` is `artist - title` when both are non-empty, else the one that is
  non-empty. An update with neither writes no line.
- Every `|`, carriage return and line feed in `<text>` is replaced by one space.
  Departure from Icecast, which writes the text unchanged: one update must be
  one line with four fields, or analysers misparse it.

Example: `14/Sep/2026:21:40:58 +0200|/live|12|Artist - Title`.

## 11. Where addresses appear

Listener addresses are personal data; the rules of resolution (proxy headers),
hashing and opting out are owned by [listeners.md](listeners.md#8-privacy). This
file only places them.

| Place                                              | Address                         |
| -------------------------------------------------- | ------------------------------- |
| Listener record (every event), stats, status page, admin page, access log | Resolved address transformed by `address_hash` |
| URL authentication requests to the backend ([url-auth.md](url-auth.md)) | Raw                            |
| The proxy-resolution function (input to hashing)   | Raw                             |
| Error log, and the server's own diagnostic log     | May contain the raw `ip:port` of a connection (as Icecast's error log does) |

With the default `address_hash`, `<ip>` is a fixed-length hexadecimal string, not
an address: analysers that validate it need that validation disabled, or
`address_hash` set to the identity.

## 12. Error log

Lines record the server's own events: sources logging in and out, rejected
sources, startup, configuration reloads.

```
[<date>] <LEVEL> <category> <message>
```

- `<date>` is `%Y-%m-%d  %H:%M:%S` in brackets, local time, with **two** spaces
  between date and time, as Icecast writes it.
- `<LEVEL>` is `EROR`, `WARN`, `INFO` or `DBUG`, for levels 1 to 4.
- `<category>` is `module/function`, reusing Icecast's where an equivalent event
  exists, so existing parsers keep matching.
- `log_level` (1 to 4, default 3) is the most verbose level written. Lower
  levels are always written.
- Values that come from clients (mount names, headers) in a message are
  escaped as in [§9](#9-access-log) for `icecast-*` and `rsas`.

Binding examples, which parsers match:

```
[2026-09-25  15:17:57] INFO connection-handle/_handle_source_request Source logging in at mountpoint "/live"
[2026-09-25  15:20:01] INFO source/source_main Source at "/live" exiting
```

Message wording beyond these is incidental.

## 13. Conformance

A suite MUST establish:

1. **Events.** Per admitted listener, exactly one connect and one later
   disconnect, with `duration` and `bytes_sent` zero then final; none for a
   refused one. Same for sources. Synchronous handlers precede
   the next stage of the producing path; an asynchronous handler that blocks does
   not delay streaming or other listeners. Binding: counts, order for
   synchronous handlers, record fields and their types. Incidental: relative
   timing of asynchronous handlers.
2. **Statistics.** After a known series of sessions, peak, connections, listening
   time, bytes sent and current listeners match; live sessions are not in the
   totals; counters survive a source reconnect; `started` changes with it.
3. **Pages.** `/` and `/status.json` exist iff `status_page` is on; refusal by
   `status_access` is `401` with the realm header; custom renderers replace the
   body; hidden mounts and addresses never appear on them. The admin paths
   answer `404` without a password, `401` without credentials, `200` with them,
   and show hashed addresses. Binding: statuses, JSON keys. Incidental: HTML
   markup, styling, refresh interval.
4. **Access log.** Exactly one line per finished admitted session; field
   by field byte equality against the format above for each `compatibility`
   value, including: escape of each special byte, kept spaces in referer and
   user agent, bytes `0x80` and up unchanged (non-`kh`), `-` for absent fields,
   truncated whole-second duration, date format with zone, one line even with
   line breaks in headers. Binding: bytes of the line. Incidental: the actual
   times.
5. **Playlist log.** `artist - title` composition, no line without text,
   `|`/CR/LF replaced by space, listener count correct.
6. **Error log.** Format and double space of the timestamp, level names, level
   filtering by `log_level`, the two binding example lines.
7. **Files.** `-` writes to standard error and never rotates; rotation happens
   after a write that leaves the size above `log_size`, at the next write;
   `.old` is replaced, archive names match `YYYYmmdd_HHMMSS`; `SIGHUP` reopens at
   the configured path; a changed target takes effect for the next line;
   concurrent writes never interleave within a line.
8. **Privacy.** With the default `address_hash` no raw listener address is found
   in events, stats, pages or the access log.
