# Parameters

The server's whole policy is one record, its **parameters**: `icecast.server`
takes it as its only argument. Every other document refers to a parameter by
the name given here. This document owns each parameter's type, default and
read point, the per-mount callback, the callbacks' inputs and outcomes, the
methods of the server value, and the reload protocol. The behaviour a
parameter controls is owned by the document linked in its row.

Types are Liquidsoap types. Examples are given as `name : type` with `?` after
the type for a nullable value. A field written `{T}` is a getter (§2.4).

## 1. Model

1. The parameters are **one record**. Every field is optional. A missing field
   takes its default. A missing field and a field set to its default are
   indistinguishable.
2. The record is **complete**: whoever builds it decides every value. The
   server merges nothing. A builder that wants "the file, except the port"
   computes that record itself ([§9](#9-overrides)).
3. A parameter is read at one of three **read points** (§2.2). A **reload**
   ([§8](#8-reload)) replaces the whole record. Some parameters also have an
   effect on what is running at the time of the reload; that is a reload
   action, not a fourth read point.
4. A parameter that is a function is a **callback**. Every function-valued
   parameter in this document is called a callback: there is no other kind.
   What each receives and may answer is in [§5](#5-callbacks).
5. Per-mount configuration is one callback, `mount` ([§4](#4-mount-parameters)),
   not a default mount plus a table.
6. What a caller can do to the running server is a **method** of the value
   `icecast.server` returns ([§6](#6-the-server-value)). Methods are not
   parameters, so a reload never drops them.

## 2. Types, units and read points

### 2.1 Units

Sizes are in bytes, durations in seconds as floats, ports and counts are
integers. A Unix path MAY be relative to the process's working directory.
"Unset" is `null`. "Unlimited" is `null` unless a row says otherwise;
`0` means zero, except where a row says `0` disables.

### 2.2 Read points

| Read point   | Meaning                                                                                      |
| ------------ | -------------------------------------------------------------------------------------------- |
| startup      | Read once when the server starts. A reload cannot change it.                                 |
| use          | Read each time the server needs it: per request, source connection, log write or event.      |
| mount output | Read when a mount output is created. An existing mount output keeps the values it was given. |

A rule that reads a parameter at "use" MUST NOT cache it across uses. A
reader sees the whole record as it stood at one instant ([§8.2](#82-atomic-swap)).

### 2.3 Where `compatibility` matters

`compatibility` chooses the implementation the server mimics wherever
implementations differ ([§3.2](#32-compatibility)). Each rule that depends on
it says what every value does, in the document that owns the rule. The server
reads it once and puts it in the requests it hands to callbacks
([§5](#5-callbacks)), so a callback or helper never receives it as an argument.

### 2.4 Getters

A parameter read at use whose type is `T` MAY be given as a getter `{T}`: a
value, or a function of no argument returning one. The server evaluates it at
each use, so a limit can change without a reload. A parameter read at startup
or at mount output creation is a plain value. A getter MUST NOT be evaluated
more than once per use. Its value is validated as the field's
([§7](#7-validation-and-errors)) when the record is validated and at each use:
a value invalid at use fails that use, as an invalid mount record does.

## 3. Server parameters

### 3.1 Startup

| Field          | Type                 | Default            | Meaning                                                                            | Behaviour owned by                                     |
| -------------- | -------------------- | ------------------ | ---------------------------------------------------------------------------------- | ------------------------------------------------------ |
| `name`         | `string`             | `"icecast.server"` | Label of the server in logs and telnet commands.                                   | [observability.md](observability.md)                   |
| `port`         | `int`                | `8000`             | TCP port for source clients and listeners. The Shoutcast protocol uses `port + 1`. | [sources.md](sources.md), [listeners.md](listeners.md) |
| `bind_address` | `string?`            | `null`             | Local address to bind. Unset: all interfaces.                                      | the same                                               |
| `transport`    | HTTP transport value | plain HTTP         | What the port speaks. TLS is a transport value ([§3.3](#33-transport-and-tls)).    | this document                                          |

### 3.2 Compatibility

| Field           | Type     | Default         | Read point | Meaning                                                                            |
| --------------- | -------- | --------------- | ---------- | ---------------------------------------------------------------------------------- |
| `compatibility` | `string` | `"icecast-2.5"` | startup    | The implementation to mimic: `"icecast-2.5"`, `"icecast-2.4"`, `"rsas"` or `"kh"`. |

Wherever implementations behave differently, the chosen one wins, including
where RSAS or KH depart from mainline. A behaviour only other implementations
have, and that conflicts with nothing the chosen one does, is honoured too.

- It is read at startup only: a reload with another value keeps the running one
  and logs the change as not applied ([§8.3](#83-after-the-swap)). Rationale:
  the record that callbacks and helpers were built for assumes one value for
  the life of the server.
- The server hands it to callbacks as `request.compatibility`
  ([§5](#5-callbacks)). The server has no getter for it.
- No Icecast configuration file sets it. The translation takes it as an
  argument, because mount selection and the CORS type depend on it, and writes
  it into the record
  ([icecast-config.md](icecast-config.md#11-rules-that-depend-on-compatibility)).

### 3.3 Transport and TLS

`transport` is the one place TLS is configured: certificate, key and their
renewal belong to the transport value, not to the server. The standard library
builds one with `http.transport.tls` or `http.transport.ssl`.

- A transport MAY carry a `reload` method. The server MUST call it at each
  reload ([§8.3](#83-after-the-swap)).
- A transport that cannot read its certificate MUST keep serving the one it has
  and log the failure. A failed certificate read is never a reload failure
  ([§8.1](#81-validate-first)): the rest of the record applies.
- A transport SHOULD also read its certificate again by itself at an interval
  (recommended 24 hours), so a renewed certificate is used without a reload.
  The retry after a failed read comes at the next connection, not after another
  interval.
- A connection already established is unaffected by a certificate change.

**Improvement (clean cut).** Icecast configures TLS through a mode per listen
socket (`disabled`, `auto`, `auto_no_plain`, `rfc2817`, `rfc2818`), spelled
differently across versions, next to several listen sockets and
`shoutcast-compat`. This server has one `transport` value on one port. It gives
the user one thing to configure and one thing to reason about; the
configuration translation maps the common spellings onto it and rejects,
with the reason, the ones it does not reproduce
([icecast-config.md](icecast-config.md#31-listen-socket)). Serving TLS and plain
HTTP on one port is listed in [future-work.md](future-work.md). This is not
subject to `compatibility`.

### 3.4 Server-wide, read at use

| Field                        | Type              | Default                                | Meaning                                                                                                                                                                                                                   | Behaviour owned by                                                 |
| ---------------------------- | ----------------- | -------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------ |
| `hostname`                   | `string`          | `"localhost"`                          | Name the server calls itself in what it tells others (URL authentication `server` field, status pages).                                                                                                                   | [url-auth.md](url-auth.md), [observability.md](observability.md)   |
| `source_password`            | `string`          | `"hackme"`                             | Password source clients present when their mount has none of its own. Empty: no credentials required. The server warns while a mount's password is `hackme` ([sources.md §3.3](sources.md#33-the-default-password)).      | [sources.md](sources.md)                                           |
| `shoutcast_mount`            | `string`          | `"/stream"`                            | Mount a Shoutcast source client feeds.                                                                                                                                                                                    | [sources.md](sources.md)                                           |
| `admin_user`                 | `string`          | `"admin"`                              | User name of the admin pages.                                                                                                                                                                                             | [observability.md](observability.md)                               |
| `admin_password`             | `string?`         | `null`                                 | Password of the admin pages. Unset: they answer as if absent.                                                                                                                                                             | [observability.md](observability.md)                               |
| `health_password`            | `string?`         | `null`                                 | Password (HTTP Basic) of `/health` and `/<mount>/health`, for mounts that have none of their own. Unset: no password.                                                                                                     | [observability.md](observability.md)                               |
| `max_history`                | `int`             | `10`                                   | Titles kept in a mount's metadata history when the mount has no `max_history` of its own, 1 to 256.                                                                                                                       | [observability.md](observability.md)                               |
| `location`                   | `string`          | `"Earth"`                              | Location shown in the status JSON and XML.                                                                                                                                                                                | [observability.md](observability.md)                               |
| `admin_contact`              | `string`          | `""`                                   | Administrator contact shown in the status JSON and XML.                                                                                                                                                                   | [observability.md](observability.md)                               |
| `server_id`                  | `string`          | a Liquidsoap identification            | Server identification shown in the status JSON and XML.                                                                                                                                                                   | [observability.md](observability.md)                               |
| `max_sources`                | `int?`            | `null`                                 | Most simultaneous source clients.                                                                                                                                                                                         | [sources.md](sources.md)                                           |
| `max_clients`                | `int?`            | `null`                                 | Most simultaneous listeners over all mounts.                                                                                                                                                                              | [listeners.md](listeners.md)                                       |
| `source_timeout`             | `float`           | `10.`                                  | Inactivity after which a source client is dropped. Read when it connects. Icecast mainline and Icecast-KH default to 10.                                                                                                  | [sources.md](sources.md)                                           |
| `proxy_ips`                  | `[string]?`       | `null`                                 | Trusted reverse proxies, as addresses or CIDR ranges. Unset: the peer address is the client's address. Empty list: trust the rightmost `X-Forwarded-For` entry ([listeners.md §8](listeners.md#8-the-listeners-address)). | [listeners.md](listeners.md)                                       |
| `forwarded_address`          | callback `?`      | `null`                                 | Computes the client's address from the request; replaces the built-in walk of `proxy_ips` ([§5.3](#53-the-address-hash)).                                                                                                 | [listeners.md](listeners.md)                                       |
| `address_hash`               | callback          | unkeyed MD5, 16 hexadecimal characters | Applied to every listener address before observers see it ([§5.3](#53-the-address-hash)).                                                                                                                                 | [listeners.md](listeners.md), [observability.md](observability.md) |
| `status_page`                | `bool`            | `true`                                 | Serve the public status endpoints. Off: they answer as unknown paths. The admin pages are not affected.                                                                                                                   | [observability.md](observability.md)                               |
| `status_access`              | callback `?`      | `null` (allow all)                     | May this request see the status endpoints ([§5.4](#54-page-callbacks)).                                                                                                                                                   | [observability.md](observability.md)                               |
| `status_json`, `status_html` | callbacks `?`     | `null` (built-in rendering)            | Replace the rendering of the status endpoints ([§5.4](#54-page-callbacks)).                                                                                                                                               | [observability.md](observability.md)                               |
| `metrics`                    | `bool`            | `false`                                | Serve the Prometheus export at `/metrics`.                                                                                                                                                                                | [observability.md](observability.md)                               |
| `access_log`                 | `string?`         | `null`                                 | Access log path; `"-"` is standard error.                                                                                                                                                                                 | [observability.md](observability.md)                               |
| `playlist_log`               | `string?`         | `null`                                 | Playlist log path; `"-"` is standard error.                                                                                                                                                                               | [observability.md](observability.md)                               |
| `error_log`                  | `string?`         | `null`                                 | Path of the Icecast-format error log, an optional sink of the server's log ([observability.md](observability.md)).                                                                                                        | [observability.md](observability.md)                               |
| `log_level`                  | `int`             | `4`                                    | Most verbose level written to the error log, on Liquidsoap's scale: 1 critical, 2 severe, 3 important, 4 info, 5 debug.                                                                                                   | [observability.md](observability.md)                               |
| `log_size`                   | `int`             | `10240000`                             | Size in bytes past which a log file rotates (Icecast's 10000 KiB).                                                                                                                                                        | [observability.md](observability.md)                               |
| `log_archive`                | `bool`            | `false`                                | Rotated logs keep a timestamped name instead of replacing the single previous copy.                                                                                                                                       | [observability.md](observability.md)                               |
| `event_bindings`             | `[event binding]` | `[]`                                   | Callbacks bound to events by configuration ([§5.5](#55-event-bindings)).                                                                                                                                                  | [observability.md](observability.md)                               |
| `mount`                      | callback          | no mount parameters                    | Per-mount parameters ([§4](#4-mount-parameters)).                                                                                                                                                                         | this document                                                      |

`log_level` 4 is the equivalent of Icecast's default `loglevel` (info); the
mapping of the two scales is owned by [observability.md](observability.md).

### 3.5 Server-wide, read at mount output creation

| Field               | Type         | Default | Meaning                                                                                                                            | Behaviour owned by                                            |
| ------------------- | ------------ | ------- | ---------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- |
| `client_timeout`    | `float`      | `30.`   | Time a listener's connection may stall before it is dropped.                                                                       | [harbor spec](../../../../core/outputs/spec/harbor/README.md) |
| `burst_on_connect`  | `bool`       | `true`  | Off: no listener gets a burst, whatever `burst_size`.                                                                              | the same                                                      |
| `burst_size`        | `int`        | `65536` | Burst for a mount's new listeners when the mount has none of its own (Icecast and Icecast-KH: 64 KiB). `0` disables the burst.     | the same                                                      |
| `http_headers`      | `[header]`   | `[]`    | Headers added to every listener reply ([§4.2](#42-headers)).                                                                       | [listeners.md](listeners.md)                                  |
| `dedicated_encoder` | `bool`       | `false` | Give each listener its own encoder.                                                                                                | [mounts.md](mounts.md)                                        |
| `muxer_options`     | callback `?` | `null`  | Muxer options for a container format ([§5.6](#56-muxer-options)). Unset: the global default ([§10](#10-existing-global-settings)). | [mounts.md](mounts.md)                                        |

## 4. Mount parameters

`mount` is a callback `(string) -> mount parameters?`. The server calls it with
a mount's path (`"/live.mp3"`) at the read point of whichever field it needs.
It answers `null` when it has nothing for that path; the server then uses its
own values and the defaults below. It never answers "no such mount": a source
client MAY create any mount the other rules allow, as on Icecast.

- The callback SHOULD be cheap, deterministic for a given record and non-blocking.
  Work done once (building a decision, a cache) belongs outside the callback,
  in the record's construction.
- The server MUST validate every record the callback returns before using it
  ([§7](#7-validation-and-errors)).
- Wildcards, defaults shared by several mounts and anything else that picks a
  mount's values are the callback's business. The Icecast configuration file
  translation builds one ([icecast-config.md](icecast-config.md#5-mount)).

### 4.1 Fields

Each is the mount's own value; when absent, the server-wide parameter of the
same name where there is one, else the default shown.

| Field                   | Type                                   | Default            | Read point             | Meaning                                                                                                                                                        | Behaviour owned by                                           |
| ----------------------- | -------------------------------------- | ------------------ | ---------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| `source_user`           | `string`                               | `"source"`         | use (source connects)  | User name source clients present.                                                                                                                              | [sources.md](sources.md)                                     |
| `source_password`       | `string`                               | the server's       | use (source connects)  | Password source clients present.                                                                                                                               | [sources.md](sources.md)                                     |
| `source_decision`       | callback `?`                           | `null`             | use (source connects)  | Replaces the user and password check ([§5.1](#51-the-source-decision)).                                                                                        | [sources.md](sources.md)                                     |
| `on_connect`            | callback `?`                           | `null`             | use (source connects)  | Called when a feeder takes the mount ([§5.2](#52-source-callbacks)).                                                                                           | [sources.md](sources.md)                                     |
| `on_disconnect`         | callback `?`                           | `null`             | use (source connects)  | Called when that source client leaves. The value read at connect is the one called.                                                                            | [sources.md](sources.md)                                     |
| `linger`                | `float?`                               | `null`             | use (feeder leaves)    | Seconds the mount output stays, listeners attached, when its feeder leaves and nothing else feeds it. Unset or `0`: no linger.                                 | [mounts.md](mounts.md#11-linger)                             |
| `max_listeners`         | `int?`                                 | `null`             | use (admission)        | Most simultaneous listeners on the mount.                                                                                                                      | [listeners.md](listeners.md)                                 |
| `listener_decision`     | callback `?`                           | `null` (admit all) | use (admission)        | Decides each listener ([§5.7](#57-decisions-and-helpers)).                                                                                                     | [listeners.md](listeners.md), [url-auth.md](url-auth.md)     |
| `max_listener_duration` | `float?`                               | `null`             | use (admission)        | Seconds after which an admitted listener is disconnected. `0` or unset: no limit.                                                                              | [listeners.md](listeners.md)                                 |
| `fallback`              | `{mount? : string, source? : source}?` | `null`             | use, and reload action | What feeds the mount while it has no source client: another mount (`mount`), or a Liquidsoap source carrying encoded content in the mount's format (`source`). | [mounts.md](mounts.md#6-fallback)                            |
| `fallback_when_full`    | `bool`                                 | `false`            | use (admission)        | A listener refused for `max_listeners` is redirected along the `fallback` chain.                                                                               | [listeners.md](listeners.md#4-listener-limits)               |
| `hidden`                | `bool`                                 | `false`            | use (status)           | Hide the mount from public status listings.                                                                                                                    | [mounts.md](mounts.md), [observability.md](observability.md) |
| `max_history`           | `int`                                  | the server's       | use (metadata)         | Titles kept in the mount's metadata history, 1 to 256.                                                                                                         | [observability.md](observability.md)                         |
| `health_password`       | `string?`                              | the server's       | use (request)          | Password (HTTP Basic) of `/<mount>/health`.                                                                                                                    | [observability.md](observability.md)                         |
| `burst_size`            | `int`                                  | the server's       | mount output           | Burst for the mount's new listeners. `0` disables it, whatever the server's value.                                                                             | the harbor spec                                              |
| `http_headers`          | `[header]`                             | `[]`               | mount output           | Headers added to the mount's listener replies, after the server's ([§4.2](#42-headers)).                                                                       | [listeners.md](listeners.md)                                 |
| `dump_file`             | `string?`                              | `null`             | use (source connects)  | File the mount's stream is recorded to; expanded with `strftime`, in local time, at each recording start.                                                      | [mounts.md](mounts.md#9-dump-file)                           |
| `dump_file_size_limit`  | `int?`                                 | `null`             | use (source connects)  | Bytes after which a recording stops. `0` or unset: no limit.                                                                                                   | [mounts.md](mounts.md#9-dump-file)                           |
| `dump_file_time_limit`  | `float?`                               | `null`             | use (source connects)  | Seconds after which a recording stops. `0` or unset: no limit.                                                                                                 | [mounts.md](mounts.md#9-dump-file)                           |

`fallback` is one record holding a mount name or a Liquidsoap source. When
both are set, validation logs a warning and the server uses `source`. The
fallback chain, its loop rule and the shutdown of a mount with nothing left to
send are [mounts.md](mounts.md)'s.

The mount's `burst_size` wins over the server's in both directions: `0` on the
mount disables the burst even when the server has one, and a value on the mount
gives a burst even when the server has `0`. `burst_on_connect` off disables the
burst of every mount.

There is no `intro` or `preroll` field. A preroll reaches the server as bytes
in the decision's outcome ([§5.7](#57-decisions-and-helpers),
[prerolls.md](prerolls.md)); the Icecast configuration file translation wraps
a mount's decision so that `intro` and `preroll` apply
([icecast-config.md](icecast-config.md#53-intro-and-preroll)).

### 4.2 Headers

`http_headers` is an ordered list. Each entry is a record
`{name: string, value?: string, compute?: callback, status?: int}`, so a value
is a string or a callback `(request, status) -> string?`:

- exactly one of `value` (sent as given) and `compute` (called per reply, see
  below) is present;
- `status`, when present, restricts the entry to replies with that status.

The final list of a reply is, in order: the headers of the listener decision,
the stream identity headers, the server's list, then the mount's list. The
server owns the whole evaluation, per reply, and this section owns the one
precedence rule:

- the first entry of a given name (compared without case) wins, whether its
  value is fixed (`value`) or computed (`compute`); later entries with that
  name are dropped, so a header is never sent twice. Rationale: a doubled
  `Access-Control-Allow-Origin` makes browsers reject the reply, and list
  order is the one thing an operator can read off;
- an entry with a `status` applies to a reply of that status only; an entry
  that does not apply, or whose `compute` answers `null`, sets no name and
  lets a later entry of that name win;
- a computed value is validated before the decision is handed to the harbor
  output: a name that is not an HTTP token, or a value with a CR or LF, drops
  the entry and is logged. The harbor output validates what reaches it too, so
  it stays the single validator of what is written;
- a `compute` that raises drops the entry and is logged, as an invalid value
  does: the reply is still sent.

`compute` is called for each reply with `(request, status)`. `request` has the
request's headers by lowercase name in `headers`, and `compatibility`
([§3.2](#32-compatibility)). `status` is the reply's. It answers a string, or
`null` to skip the entry. It exists because some headers depend on the request
(a CORS origin) and on the reply's status. The ready-made CORS header set is
`icecast.server.headers.cors` ([§5.7](#57-decisions-and-helpers)).

The server wraps every decision, so it builds the complete reply head of a
refusal, a redirect and a decision failure, with the final header list. For an
admitted listener it hands the harbor output the headers to add to the `200`
reply (the `headers` field of the harbor decision). The harbor
output writes both as given and de-duplicates nothing
([connection.md §4](../../../../core/outputs/spec/harbor/connection.md#4-admission-predicate)).

## 5. Callbacks

A callback is called on work that may block unless a row says otherwise. A
callback that raises is a failure of the use that called it; the failure's
effect is stated with each callback.

**What the server hands in.** Ready-made decisions and helpers are built by the
script, outside the server, and built again for each `reload`. They are not
asked to know what only the server knows. The server reads these once and puts
them in the records it passes:

| Field           | Type                                                 | In                                                                                   | Content                                                                                                                                                                    |
| --------------- | ---------------------------------------------------- | ------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `server`        | `{hostname : string, port : int, instance : string}` | the listener and source decision requests, the source event records                  | `hostname` and `port` as [§3](#3-server-parameters) gives them; `instance` is a UUID the server generates once when it starts and keeps across reloads: fixed for its life |
| `compatibility` | `string`                                             | the listener and source decision requests, the `compute` input ([§4.2](#42-headers)) | The value of [§3.2](#32-compatibility).                                                                                                                                    |

### 5.1 The source decision

`source_decision` is a callback `(request) -> bool`, called after the reserved
path check and in place of the password check
([sources.md](sources.md#31-the-source-decision)). Its input is
[sources.md §3.1](sources.md#31-the-source-decision)'s request, which carries
`server` and `compatibility` ([§5](#5-callbacks)).

Outcome: `true` admits, `false` refuses as bad credentials. An error it
raises refuses the same way and is logged. The statuses are
[sources.md](sources.md#4-replies)'s.

### 5.2 Source callbacks

`on_connect` is a callback `(source-connect event) -> unit` and `on_disconnect`
a callback `(source-disconnect event) -> unit`; the arguments are the records of
the source events ([observability.md](observability.md#1-events)), the connect
one carrying the Liquidsoap source of the feed. They run in the background; their duration
never delays the stream. A callback
that raises is logged and changes nothing. They replace Icecast's command
strings: the Icecast configuration file translation wraps a command into one.

### 5.3 The address hash

`address_hash` is a callback `(string) -> string` from an address to the string
observers see. Its rules are [listeners.md §9](listeners.md#9-privacy).

`forwarded_address` is a callback `(request) -> string?` that resolves a
client's address behind a reverse proxy. Its input:

| Field       | Content                                           |
| ----------- | ------------------------------------------------- |
| `ip`        | The peer address of the connection.               |
| `headers`   | The request headers, looked up by lowercase name. |
| `protocol`  | The HTTP version of the request.                  |
| `uri`       | The requested path.                               |
| `proxy_ips` | The parameter of that name, `null` when unset.    |

It answers the client's address, or `null` to use the peer address. When set it
replaces the walk of `proxy_ips`; the rules of both are
[listeners.md §8](listeners.md#8-the-listeners-address). An error it raises is
logged and gives the peer address. It is read at use.

### 5.4 Page callbacks

`status_access` is a callback `(request) -> bool`: `false` refuses with `401`.
`status_json` and `status_html` are callbacks `(stats) -> string`, where
`stats` is the list `stats()` returns ([§6](#6-the-server-value)). The pages
are ordinary HTTP handlers on the server's port, so a script MAY also put
middleware in front of them; [observability.md](observability.md) owns them.

### 5.5 Event bindings

An event binding is `{event: string, mount?: string, synchronous?: bool,
handler: callback}`: `handler` is called with the event's record, as a handler
registered with the method of the same event ([§6](#6-the-server-value)). `event`
is `"source_connect"`, `"source_disconnect"`, `"listener_connect"`,
`"listener_disconnect"` or `"metadata"`; `mount` restricts the binding to one
mount; `synchronous` defaults to `false`. Bindings are part of the record, so a
reload replaces them. Handlers registered by method are not touched.

### 5.6 Muxer options

`muxer_options` is a callback `(string) -> [(string * string)]` from a
container format name (`"matroska"`) to the muxer options used when a stream
is remuxed.

### 5.7 Decisions and helpers

`listener_decision` is a callback from a listener request to an outcome. The
input is [listeners.md §2](listeners.md#2-the-listener-decision) and the check
order [listeners.md §1](listeners.md#1-the-admission-pipeline).
The outcome is a record, every field optional:

| Field        | Type                         | Meaning                                                                                                                                                                                                     |
| ------------ | ---------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `refuse`     | `{status, reason, headers?}` | Refuse the listener, or redirect it when the status is a redirect and `headers` carry `Location`. This is harbor's admission record.                                                                        |
| `time_limit` | `float`                      | Seconds after which the listener is disconnected. Harbor's field.                                                                                                                                           |
| `preroll`    | `string`                     | Bytes sent before the stream. Harbor's field. A Liquidsoap source cannot be a preroll: it would need encoding.                                                                                              |
| `exempt`     | `bool`                       | Skips `max_listeners` and `fallback_when_full`; still counted in, and subject to, `max_clients` (KH `icecast-slave`).                                                                                       |
| `on_release` | `(release) -> unit`          | Called once with the release record when the admitted listener leaves, or when its admission ends before it is served: refused by a later step, rejected by the harbor output, or the mount output stopped. |
| `user`       | `string`                     | The listener's new user name, from a backend rename: the user the per-user limit counts ([listeners.md §7.3](listeners.md#73-per-user-connection-limit)).                                                   |

The input is [listeners.md §2](listeners.md#2-the-listener-decision)'s request.
It carries `server`, `compatibility` and `user_listeners` (a function from a
user name to that user's admitted listeners on the mount) besides the
request's own fields, so a decision built by a helper needs no argument for
them. The release record is:

| Field        | Type                  | Meaning                                                                                                                                         |
| ------------ | --------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `duration`   | `float`               | The session's length in seconds; `0` for a listener whose admission ended before it was served.                                                 |
| `bytes_sent` | `int`                 | Bytes sent to the listener, as the listener record's ([observability.md](observability.md#2-the-listener-record)); `0` then.                    |
| `listeners`  | `int`                 | The mount's listener count at release, this listener included (RSAS sends it so); without it for a duration-0 release, which was never counted. |
| `metadata`   | `[(string * string)]` | The mount's current metadata at release.                                                                                                        |

A decision is told when its listener is released through `on_release` only: the
closure it returned at admission is the one called, whatever a reload changed
meanwhile. The server sets the harbor decision's `on_dropped` to call
`on_release` with a duration-0 record
([connection.md §4](../../../../core/outputs/spec/harbor/connection.md#4-admission-predicate)),
so an admission that the harbor output does not complete releases what the
decision reserved.

The standard library provides these helpers. Their behaviour is owned by the
document linked; the names are fixed here.

| Public helper                                                                                        | Builds                                                                                                                                                                            | Owner                                                |
| ---------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| `icecast.server.config.load(~compatibility="icecast-2.5", path)`                                     | The parameters of an Icecast, RSAS or Icecast-KH file. `compatibility` is an argument because the translation branches on it, and it is written into the record.                  | [icecast-config.md](icecast-config.md)               |
| `icecast.server.refusal(~reason, ~headers, status)`                                                  | A `refuse` value.                                                                                                                                                                 | [listeners.md](listeners.md#2-the-listener-decision) |
| `icecast.server.redirect(status, location)`                                                          | A `refuse` value that redirects.                                                                                                                                                  | the same                                             |
| `icecast.server.decision.all(decisions)`                                                             | A decision composing others in order, and the one owner of how their outcomes merge ([listeners.md §7](listeners.md#7-ready-made-decisions)).                                     | the same                                             |
| `icecast.server.decision.url_auth(~cache, options)`                                                  | URL authentication; `options` are url-auth.md's option names, `cache` is the preroll cache that loads the prerolls the backend chooses and bounds them by its `preroll_max_size`. | [url-auth.md](url-auth.md)                           |
| `icecast.server.decision.access_lists(~deny, ~allow, ~agents)`                                       | Access lists; each argument is a file name or `null`.                                                                                                                             | [listeners.md](listeners.md#71-access-lists)         |
| `icecast.server.decision.htpasswd(file)`                                                             | Basic authentication against a file.                                                                                                                                              | [listeners.md](listeners.md#72-htpasswd)             |
| `icecast.server.headers.cors(~origins=[], ~expose=[])`                                               | The CORS `http_headers` entries. `origins` restricts the allowed origins (empty: any) and `expose` appends names to the exposed headers, under every `compatibility` value.       | [listeners.md](listeners.md#32-per-reply-headers)    |
| `icecast.server.preroll_cache(~directory, ~preroll_timeout, ~preroll_max_size, ~preroll_revalidate)` | A view over the process-wide preroll store that turns a location into bytes; `directory` is the base directory of local locations.                                                | [prerolls.md](prerolls.md#3-the-cache)               |

These helpers are internal. `icecast.server.config.load` calls them, and no
script needs them: `connections_per_user(n)` (the per-user limit,
[listeners.md §7.3](listeners.md#73-per-user-connection-limit)),
`source_callbacks.url(options)` (the `mount_add` and `mount_remove` notifications,
[sources.md §7](sources.md#7-notifications)) and `source_decision.stream_auth(options)`
(URL authentication of source clients,
[sources.md §3.2](sources.md#32-url-authentication-of-source-clients)). A script
that wants their behaviour loads a file, or writes its own callback.

No helper takes `compatibility`, `hostname`, `port` or the server instance as an
argument: each reads them from the request it is called with
([§5](#5-callbacks)).

## 6. The server value

`icecast.server(parameters)` starts the server and returns a value with these
methods. A reload never drops them.

| Method                                                | Meaning                                                                                                                                                     |
| ----------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `on_source_connect(~synchronous=false, handler)`      | Register a handler for a source client connecting.                                                                                                          |
| `on_source_disconnect(~synchronous=false, handler)`   | The same, for a source client leaving.                                                                                                                      |
| `on_listener_connect(~synchronous=false, handler)`    | The same, for a listener admitted.                                                                                                                          |
| `on_listener_disconnect(~synchronous=false, handler)` | The same, for a listener gone.                                                                                                                              |
| `on_metadata(~synchronous=false, handler)`            | The same, for a metadata update of a mount.                                                                                                                 |
| `stats()`                                             | A snapshot of the statistics, one entry per mount ([observability.md](observability.md#5-the-stats-snapshot)).                                              |
| `reload(parameters)`                                  | Replace the whole record ([§8](#8-reload)).                                                                                                                 |
| `reopen_logs()`                                       | Close and open again every log file ([observability.md](observability.md)).                                                                                 |
| `attach_source(~headers=[], mount, source)`           | Feed a mount from a Liquidsoap source of encoded content ([mounts.md](mounts.md#2-what-feeds-a-mount)). `headers` are the optional stream identity headers. |
| `detach_source(mount)`                                | Stop feeding the mount from the attached source.                                                                                                            |

- A handler registered with `synchronous=true` is called in order with the
  event; with `false` it MAY run in parallel with others, so state it shares
  needs atomic operations. The events, their records and their ordering are
  [observability.md](observability.md#3-ordering-guarantees)'s.
- Binding `reload` and `reopen_logs` to signals (`SIGHUP`, `SIGUSR1`) or to a
  timer is the script's: the server does not own signals.
- `attach_source` raises `error.icecast.server.mount_taken` when the mount has a
  feeder, `error.icecast.server.reserved_path` on a reserved path
  ([sources.md](sources.md#2-reserved-paths)) and
  `error.icecast.server.format_mismatch` when the content does not match the
  mount's format ([mounts.md](mounts.md#3-the-format-match-rule)).
- The server value has no method that changes a single parameter. The only way
  to change a parameter is `reload` with a whole record.

## 7. Validation and errors

A record is **valid** when:

- `compatibility` is one of `"icecast-2.5"`, `"icecast-2.4"`, `"rsas"`, `"kh"`;
- `port` is in 0 to 65535; counts, sizes and durations are not negative and
  durations are finite; `log_level` is in 1 to 5; `max_history`, server-wide
  and per mount, is in 1 to 256 (an Icecast value outside it is mapped by the
  translation, [icecast-config.md](icecast-config.md#5-mount));
- each `http_headers` entry has exactly one of `value` and `compute`, a
  non-empty `name` that is an HTTP token, a `value` without CR or LF, and a
  `status`, when present, in 100 to 599. The value a `compute` answers is
  checked at each reply ([§4.2](#42-headers));
- each event binding names one of the five events;
- each `proxy_ips` entry is an IPv4 or IPv6 address, or one in CIDR notation
  (`address/prefix`, with a prefix no longer than the address);
- a mount record's `fallback` is `{mount?, source?}`, with a `mount` that starts
  with `/`. A `fallback` with both fields is valid: it logs a warning and uses
  `source`.

A mount record is validated each time `mount` returns it. When a use finds it
invalid, that use fails: the source client or listener is refused as the
failing callback would be ([§5](#5-callbacks)), and the failure is logged.

Validation and reload failures raise `error.icecast.server.invalid_parameters`.
Its message names the field, the mount when there is one, and why. The other
kinds are `error.icecast.server.mount_taken`, `error.icecast.server.reserved_path`
and `error.icecast.server.format_mismatch`, raised by `attach_source`.

- An invalid record MUST be rejected at startup (the server does not start) and
  at reload ([§8.1](#81-validate-first)).
- An invalid value MUST NOT be discovered lazily at the first request that
  reads it, for every field of the server record.

## 8. Reload

`reload(parameters)` replaces the whole record. Calls are serialised: a second
call waits for the first.

### 8.1 Validate first

The new record MUST be fully validated ([§7](#7-validation-and-errors)) before
anything changes. The validation includes the record `mount` returns for every
mount that is live. When validation fails, `reload` raises
`error.icecast.server.invalid_parameters` and leaves the previous record, every
mount and every connection unchanged. A server never runs a half-applied
record.

### 8.2 Atomic swap

On success the new record replaces the old one in a single step. A reader MUST
see either all of the old record or all of the new one, never a mixture. A
request in progress finishes with the values it already read.

### 8.3 After the swap

| Class of parameter | Effect                                                                                                            |
| ------------------ | ----------------------------------------------------------------------------------------------------------------- |
| startup            | Kept. The server logs, for each one whose value changed, that it was not applied. `compatibility` is one of them. |
| use                | Applies at the next use: the next request, source connection, log write or event.                                 |
| mount output       | Applies when a mount output is next created. Existing outputs are not touched.                                    |

The reload also performs these actions before it returns:

- `transport.reload()` is called when the transport has one. Its failure is
  logged and is not a reload failure.
- A live mount follows its new `fallback` at once. The `source` of a
  `fallback` is the same Liquidsoap source as before ([§9](#9-overrides)), so
  the mount keeps following it. A mount left with nothing to send under any source is shut down, as
  [mounts.md](mounts.md#1-lifecycle) states.
- `event_bindings` are replaced.
- The run-time overlay ([§8.4](#84-run-time-overlay)) is cleared.
- The warning about the default source password
  ([sources.md §3.3](sources.md#33-the-default-password)) is logged again when
  it still applies.

A reload MUST NOT disconnect a source client or a listener otherwise. A
failure in an action is logged and never makes the reload fail: the record is
already in force.

### 8.4 Run-time overlay

The `fallbacks` admin request ([mounts.md §6.3](mounts.md#63-fallbacks-admin-request))
sets a mount's fallback at run time. The server keeps these in an **overlay**:
state held by the server, never part of the record, consulted before the
`fallback` the `mount` callback gives. An entry is `mount path -> fallback`
(with `mount` set; an empty request value is an entry that clears the fallback).
An entry lives until the mount output ends or a reload, as
[mounts.md §6.3](mounts.md#63-fallbacks-admin-request) states; under `kh` an
entry for a mount that is not live lives until a reload. A reload clears the
overlay, so the new record is the only source of policy afterwards. The overlay
is not a parameter and cannot be set by a script.

## 9. Overrides

An override is a record substitution made by the script, never a feature of
the server:

```liquidsoap
def parameters() =
  icecast.server.config.load(compatibility="icecast-2.5", "icecast.xml").{port = 8100}
end
server = icecast.server(parameters())
server.reload(parameters())
```

A script that overrides a value MUST override it on every reload, so that a
reload does not silently undo it. Computing the record in one function, as
above, does that. The server does no merging of its own.

The function runs again at every reload, so it MUST NOT create a Liquidsoap
source: one in a `fallback`, or one the script attaches. A source created in
the function is a new source with its own clock state at each call, and "the
mount follows its new `fallback`" compares sources by identity. The script
creates each Liquidsoap source once, outside the function, and the function
refers to it.

```liquidsoap
fallback_source = (...)  # created once
def parameters() =
  (...).{mount = fun (path) -> {fallback = {source = fallback_source}}}
end
```

## 10. Existing global settings

The global `settings.icecast.server.*` settings are process-wide defaults, not
the server's parameters.

| Global setting                                  | Stays or moves | Notes                                                                                                                              |
| ----------------------------------------------- | -------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| `settings.icecast.server.tls_transport`         | stays          | Chooses the factory that `icecast.server.config.load` uses to build `transport`. Not read by the server itself.                    |
| `settings.icecast.server.default_muxer_options` | stays          | Default of `muxer_options` ([§5.6](#56-muxer-options)).                                                                            |
| `settings.icecast.server.preroll_cache_size`    | stays          | Bytes bounding the one process-wide preroll store, read each time a preroll is stored ([prerolls.md §3](prerolls.md#3-the-cache)). |
| `settings.icecast.server.status_codes`          | moves          | Replaced by the `compatibility` parameter, which also decides everything else that differs between implementations.                |

The preroll store lives in the process, not in the server: a reload, or a new
`icecast.server.config.load`, keeps it warm. The limits of one use
(`preroll_max_size`, `preroll_revalidate`, `preroll_timeout`) are arguments of
`icecast.server.preroll_cache`, which returns a view over the store, not
parameters of the server ([prerolls.md](prerolls.md)).

## 11. Conformance

**Binding**

- Every field of §3 and §4 takes its default when missing, and a record with a
  field set to its default behaves exactly like one without it.
- `compatibility` outside its four values, and every other invalid value of
  §7, make startup fail and `reload` raise `error.icecast.server.invalid_parameters`,
  leaving the previous record in force: a request after the failed reload sees
  the old values, sources and listeners stay connected, mounts stay.
- After a successful reload, a request made after it sees every use-class value
  new, one made before saw every one old, and none sees a mixture (test with two
  parameters changed together and a request loop).
- A startup-class change is not applied (the port keeps serving) and is logged
  as not applied; a reload with another `compatibility` is one.
- Requests carry what the server knows: a decision and a `compute` header see
  the `compatibility` the server started with, and the same `server.instance`
  across a reload.
- A mount output created before a reload keeps its `client_timeout`, burst and
  headers; one created after has the new ones.
- A fallback set by the `fallbacks` request holds until a reload, which clears
  it; a `fallback` with both `mount` and `source` logs a warning and uses
  `source`.
- `proxy_ips` unset gives the peer address, an empty list trusts the rightmost
  `X-Forwarded-For` entry, and `forwarded_address` when set replaces the walk
  and is read at each use.
- `linger` is read when the feeder leaves; a changed value applies to the next
  departure.
- Header precedence is [§4.2](#42-headers)'s: the decision's headers, the
  stream identity headers, the server's list, then the mount's; the first entry
  of a name wins, fixed or computed, and later ones are dropped. An entry that
  does not apply or answers `null` lets a later one win. A computed value with
  a line break, or a name that is not a token, is dropped and logged.
- A `fallback.source` created once outside the function that builds the record
  is the one a live mount keeps following after a reload.
- A live mount follows a changed fallback during the reload; a mount whose
  fallback was removed and that has no source client is gone when `reload`
  returns.
- A mount's own value beats the server's of the same name, which beats the
  default; headers follow [§4.2](#42-headers).
- A mount's `burst_size` of `0` disables the burst when the server's is not
  `0`, and a mount's positive value gives one when the server's is `0`.
- A getter given for a use-class field is evaluated at each use, and its changes
  apply without a reload.
- `source_decision` outcomes map to the replies of [sources.md](sources.md#4-replies);
  `on_release` is called exactly once per admitted listener, with a release
  record of the session's `duration`, `bytes_sent`, `listeners` and `metadata`,
  also for an admission that ended before the listener was served
  (duration 0, `listeners` without it).
- Header entries are evaluated by the server: a refusal, a redirect and a
  decision failure that raised reach the harbor output with their final header
  list, and an admitted listener's `200` reply carries the entries as the
  decision's admit headers.
- Handlers registered with `on_*` before a reload still fire after it;
  `event_bindings` of the old record do not.
- `reload` and `reopen_logs` are methods; nothing in the server installs a
  signal handler.
- The preroll store is kept by a reload (a cached preroll is not fetched again).
- A record built by substitution over `icecast.server.config.load` and passed
  to both `icecast.server` and `reload` keeps the substituted value.

**Incidental**

- Log wording, the moment within the interval at which a transport reads its
  certificate, the text of an error message beyond naming the field.

**Doubles.** A transport value that counts `reload` calls and can fail on
demand; a callback `mount` that counts its calls; a fake backend for the
decisions.

**Something ran.** Assert the number of reloads that succeeded and that failed,
the number of requests made on each side of a reload, and fail when any is
zero.
