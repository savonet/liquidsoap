# Sources: admission, content and lifecycle

A **source client** is a network client that sends an encoded stream to a
mount. This document states how the server admits one, what it answers, what
it keeps of the stream and what happens when it goes away. A **Liquidsoap
source** is a source of the script running the server; it can feed a mount too
([mounts.md §2](mounts.md#2-what-feeds-a-mount)). "Feeder" means either one.
The mount a feeder serves is specified in [mounts.md](mounts.md); delivery to
listeners belongs to the
[harbor output](../../../../core/outputs/spec/harbor/icecast-server.md).

Parameters are defined in [parameters.md](parameters.md). Where
implementations differ, the `compatibility` parameter (`"icecast-2.5"` default,
`"icecast-2.4"`, `"rsas"`, `"kh"`) chooses the one to mimic; a rule that
depends on it says what each value does.

## 1. Protocols

The server MUST accept a source client over:

| Protocol  | Request                                                       | Port       |
| --------- | ------------------------------------------------------------- | ---------- |
| HTTP PUT  | `PUT <mount>` with `Content-Type` and Basic credentials       | `port`     |
| SOURCE    | `SOURCE <mount> HTTP/1.0` (Icecast's legacy method), Basic    | `port`     |
| Shoutcast | the password on a line, then `icy-*` headers (the ICY source) | `port` + 1 |

The server SHOULD also accept `POST` as `PUT` and the legacy
`SOURCE <password> <mount> ICE/1.0` form. A client that sends
`Transfer-Encoding: chunked` is read as chunked. A mount path MUST start with
`/`; any other path is refused with `400`.

**Shoutcast.** A Shoutcast source client has no mount in its request. It feeds
the mount named by `shoutcast_mount` (Icecast's `<shoutcast-mount>`), and the
line may carry `user:password`. The server answers `OK2`, `icy-caps:11` and an
empty line, then reads the `icy-*` headers. This rule holds under every
`compatibility` value. The Shoutcast admin URL (`/admin.cgi`,
[§6.1](#61-metadata)) addresses the same mount.

**Content type.** A `PUT` or `SOURCE` without `Content-Type` is refused
([§4](#4-replies)). A Shoutcast source client without one is taken as
`audio/mpeg`.

## 2. Reserved paths

These paths are never mounts: a source client on one is refused
([§4](#4-replies)), and a Liquidsoap source cannot be attached to one.

- `/`, `/status.json`, `/status-json.xsl`;
- `/admin.cgi`, `/admin` and every path under `/admin/`;
- `/metrics`;
- `/health`, and every path ending in `/health` (`/<mount>/health` is a page,
  [observability.md](observability.md), so a mount name cannot end so);
- every other path the server serves as a page or an endpoint
  ([observability.md](observability.md)).

Rationale: a mount output on such a path would shadow the page. The set does
not depend on whether a page is switched on: turning a page off does not turn
its path into a mount.

## 3. Admission

A source client's request is judged in this order. The first failing check
decides the reply.

1. **Reserved path** ([§2](#2-reserved-paths)).
2. **Credentials**, by the mount's `source_decision` when it has one
   ([§3.1](#31-the-source-decision)), otherwise by password:
   the user and password come from HTTP Basic credentials, and the mount's
   `source_user` and `source_password` apply, each falling back to the server's.
   When the resulting password is empty, no credentials are required.
   Rationale: Icecast's per-mount `<username>` and `<password>` override the
   global source password.
3. **Mount taken.** A mount that already has a feeder refuses another source
   client. The check comes after authentication, so an unauthenticated client
   learns nothing about live mounts.
4. **`max_sources`.** When set, a source client is refused while that many
   mounts have a source client. Unset means unlimited. Liquidsoap sources are
   the script's own: they are not counted and not admitted by this procedure.

A mount that exists only as another mount's fallback has no feeder
([mounts.md §1](mounts.md#1-lifecycle)): a source client MAY take it.

A source client is never refused for the format of its stream. When the mount
has an output of another format, the output is replaced or continued as
[mounts.md §3](mounts.md#3-the-format-match-rule) states.

### 3.1 The source decision

`source_decision` is a callback, per mount, that replaces the password check
of step 2. It receives:

| Field           | Content                                                                                                   |
| --------------- | --------------------------------------------------------------------------------------------------------- |
| `address`       | The client's address as resolved in [listeners.md §8](listeners.md#8-the-listeners-address), never hashed |
| `method`        | `PUT`, `POST`, `SOURCE` or `ICY`                                                                          |
| `mount`         | The mount path, query string excluded                                                                     |
| `query`         | The query string as received, empty when none                                                             |
| `user`          | The Basic user name, empty when none                                                                      |
| `password`      | The Basic password, empty when none                                                                       |
| `admin`         | `true` when the request is an admin request on the mount ([§6](#6-admin-requests)), `false` for a stream  |
| `agent`         | The `User-Agent` header, `-` when absent                                                                  |
| `headers`       | The request headers, looked up by lowercase name                                                          |
| `server`        | `{hostname, port, instance}` ([parameters.md §5](parameters.md#5-callbacks))                              |
| `compatibility` | The server's `compatibility` ([parameters.md §3.2](parameters.md#32-compatibility))                       |

The server reads `server` and `compatibility` and hands them in, so a ready-made
decision, built outside the server and again at each reload, takes no argument
for them. A source decision that needs the client's agent or headers (URL
authentication does) reads them from the request.

Its outcome is admit or refuse. A refusal is answered as bad credentials
([§4](#4-replies)): `401` with the challenge. The callback MUST NOT be called
for a request that failed step 1. An exception it raises refuses the source
client and is logged.

A source decision does not replace the checks of steps 3 and 4.

### 3.2 URL authentication of source clients

`stream_auth` (Icecast-KH; Icecast mainline's `stream_auth` option of
`<authentication type="url">`) is a ready-made source decision: the server
asks the operator's web service. It is honoured under every `compatibility`
value, since it conflicts with nothing. It is an internal helper
(`source_decision.stream_auth`) that the configuration translation uses
([parameters.md §5.7](parameters.md#57-decisions-and-helpers)).

The request is a `POST` of form fields, with the credentials, `timeout`,
failure handling and reply headers of [url-auth.md](url-auth.md#3-credentials)
and [url-auth.md §7](url-auth.md#7-failures). Fields: `action`
(`stream_auth`), `server`, `port` and `server-instance` (the request's
`server`), `mount`, `ip` (the request's `address`), `agent`, `user`, `pass`, and
`admin=1` for an admin request. The reply
admits the source client under the rule of
[url-auth.md §4](url-auth.md#4-the-reply) for admission; time limits,
redirects and prerolls do not apply. Any other reply refuses as bad
credentials, unless it carries `icecast-resp-status` (300 to 599) and
`icecast-resp-reason`: they set the refusal's status and reason, as in
[url-auth.md §5.3](url-auth.md#53-refusal). RSAS's webhooks use them; every
`compatibility` value honours them.

### 3.3 The default password

`source_password` defaults to `hackme`, Icecast's historical default, so a
server started with no record still accepts source clients. The server MUST
log a warning at startup, and again at each reload
([parameters.md §8.3](parameters.md#83-after-the-swap)), while any mount's
source password is `hackme`. Rationale: the password also unlocks the mount
commands of [§6](#6-admin-requests), such as `killsource`.

## 4. Replies

| Refusal           | `icecast-2.5` | `icecast-2.4`, `kh`, `rsas` |
| ----------------- | ------------- | --------------------------- |
| bad credentials   | `401`         | `401`                       |
| mount taken       | `409`         | `403 Mountpoint in use`     |
| `max_sources`     | `503`         | `403 Too many sources`      |
| no `Content-Type` | `400`         | `400`                       |
| unparsable stream | `415`         | `403`                       |
| reserved path     | `403`         | `403`                       |

Under `rsas`, bad credentials (`401`) and a mount taken (`403 Mountpoint in
use`) are RSAS's; it does not document the other refusals, so their statuses
are this server's rule, those of `icecast-2.4`.

A `401` MUST carry `WWW-Authenticate: Basic realm="Icecast2 Server"`, so source
clients that wait for the challenge before sending credentials (several do)
retry correctly. Rationale: Icecast's own realm; clients do not read it.
A refused source client gets a short body and the connection is closed.

An admitted source client is answered:

- `SOURCE` and Shoutcast: `200 OK` (Shoutcast: the `OK2` form above) at once;
- `PUT`: `100 Continue` when the client sent `Expect: 100-continue`, otherwise
  nothing until the first data is read, as Icecast does.

## 5. Stream identity and content

A source client describes its stream with headers: `ice-name`,
`ice-description`, `ice-genre`, `ice-url`, `ice-bitrate`, `ice-public` over
HTTP, or `icy-name`, `icy-description`, `icy-genre`, `icy-url`, `icy-br`,
`icy-pub` over Shoutcast and old clients.

The server MUST pass them to listeners as `icy-name`, `icy-description`,
`icy-genre`, `icy-url`, `icy-br` and `icy-pub` in the response of the mount.
When a source client sends both forms the first one received wins. A value
containing a line break MUST be dropped: it would end the response head.
The identity belongs to the mount's current feeder, not to the mount output:
the server builds the `icy-*` headers for each listener reply from the
feeder at that moment, so a listener that joins after a source client
reconnected with other headers gets the new ones. What the identity is while
no source client feeds the mount is [mounts.md §4](mounts.md#4-the-mount-output)'s.

The server MUST NOT decode or re-encode a stream. It splits it into encoded
packets and writes them again in the container the source client used (MP3,
AAC, Ogg with Opus, Vorbis or FLAC, or any container it sent), so a stream is
relayed at the cost of a copy. Rationale: listeners get what the source client
produced, and a server holds many mounts.

- The container's `muxer_options` (per container name) apply to the mount's
  output.
- The mount's MIME type is the type the source client declared.
- A source client that switches to another stream mid-connection (Ogg
  chaining, new MP3 or ADTS parameters, a codec change) MUST keep the
  connection. The mount follows it as [mounts.md §3](mounts.md#3-the-format-match-rule)
  states. Whether a new link starts a new track is the output's
  ([harbor spec](../../../../core/outputs/spec/harbor/icecast-server.md)).
- A stream the server cannot parse is refused as unparsable ([§4](#4-replies))
  when detected at login, and otherwise ends the connection like a
  disconnection.

## 6. Admin requests

This section owns who may issue an admin request and what it answers when the
request is malformed. Two kinds exist.

**Mount commands** act on the mount named by the `mount` parameter: `metadata`,
`killsource`, `manage` (RSAS), `fallbacks` ([mounts.md §6.3](mounts.md#63-fallbacks-admin-request))
and `/admin/listclients` ([observability.md §7.3](observability.md#73-icecast-xml)).
They are authenticated as the mount's source client would be ([§3](#3-admission)
step 2, with `admin` true for `source_decision`), by Basic credentials or, on
`/admin.cgi`, by the `user` and `pass` query parameters. The admin user and
password ([parameters.md](parameters.md)) are accepted too when `admin_password`
is set. A mount command is served whether or not `admin_password` is set.
Rationale: Icecast grants the commands of a mount to its sources.

**Admin pages** (`/admin/stats`, `/admin/listmounts`, `/admin/listeners`,
`/admin/listeners.json`; [observability.md §7](observability.md#7-the-admin-pages))
accept the admin credentials only. While `admin_password` is unset no
credentials can match: they answer `401` with the challenge of
[§4](#4-replies), whatever the request carries, as Icecast does for a request
without valid credentials. The server logs a warning at startup, while
`admin_password` is unset, that the admin pages refuse everyone.

Departure from Icecast 2.5, whose legacy source roles are granted admin access
(a source client's credentials may open an admin page there): here only the
admin credentials open an admin page, under every `compatibility` value.
Rationale: a source password is shared with encoders and should not read
listener data.

Errors of a mount command, per `compatibility`. RSAS has no Icecast admin
interface (it documents `/admin/metadata`, `/admin/stats`,
`/admin/listclients`, `/admin/listmounts` and `POST /<mount>/manage`) and
documents none of these errors, so `rsas` follows `icecast-2.4` here as this
server's rule:

| Condition                                           | `icecast-2.5` | `icecast-2.4`, `kh`, `rsas` |
| --------------------------------------------------- | ------------- | --------------------------- |
| no `mount` parameter                                | `400`         | `400`                       |
| unknown mount                                       | `404`         | `400`                       |
| the command acts on a feeder and the mount has none | `400`         | `400`                       |
| bad credentials                                     | `401`         | `401`                       |

`401` carries the challenge of [§4](#4-replies).

### 6.1 Metadata

`GET /admin/metadata` (Icecast) and `GET /admin.cgi` (Shoutcast) with
`mode=updinfo`, `mount` (on `/admin.cgi`, absent means `shoutcast_mount`), and
`song`, or `title` and `artist`, `url`, `charset`.

1. Without `mode=updinfo` the reply is `400`.
2. Only mounts whose container has no in-stream metadata take it: MP3, AAC,
   WAV and FLAC. Others get `405`.
3. `song` is read as the title. When `title` or `artist` is also given, `song`
   is dropped. `url` becomes the track's URL. Text is converted from `charset`
   (else the source client's charset) to UTF-8.
4. The reply is `200`; Icecast's body is XML (`<iceresponse>`).
5. It applies to the mount whatever feeds it.

Rationale for rule 2: the metadata of the other containers travels in the
stream. The update reaches listeners and the playlist log as any metadata
([mounts.md §8](mounts.md#8-metadata), [observability.md](observability.md)).

### 6.2 `killsource`

`GET /admin/killsource?mount=<mount>` (Icecast, Icecast-KH) and
`POST /<mount>/manage` with `action=kick` (RSAS) disconnect the mount's source
client. Both are served under every `compatibility` value: neither conflicts
with the other. The effect is that
of a disconnection by the client ([§8](#8-lifecycle)); the reply is `200` with
Icecast's `<iceresponse>` body ("Source Removed"). A mount fed by a Liquidsoap
source has no source client to act on: the request gets the error of
[§6](#6-admin-requests) for a mount without a feeder to act on.
Rationale: detaching a Liquidsoap source is the script's decision
([mounts.md §2](mounts.md#2-what-feeds-a-mount)).

## 7. Notifications

Icecast's `mount_add` and `mount_remove` (mainline, Icecast-KH) notify a URL
when a mount's feeder connects and leaves. The ready-made
`icecast.server.source_callbacks.url` ([parameters.md](parameters.md#57-decisions-and-helpers))
returns the mount's `on_connect` and `on_disconnect` callbacks that `POST` form
fields to the operator's URL, with the credentials and failure handling of
[url-auth.md §3](url-auth.md#3-credentials) and [§7](url-auth.md#7-failures);
the reply is ignored and a failure is logged. They never block the stream.
The helper is internal (`source_callbacks.url`): the configuration translation
uses it. It reads everything it sends from the source event record
([observability.md §1](observability.md#1-events)).

When a mount has these notifications and also `on_connect` or `on_disconnect`
commands (an Icecast configuration can have both,
[icecast-config.md](icecast-config.md#51-authentication)), the mount's callback
runs both, the notification first. Each is independent: a failure of one does
not stop the other.

| Field             | `mount_add`, `mount_remove`                                                                                                                           |
| ----------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| `action`          | `mount_add` or `mount_remove`                                                                                                                         |
| `mount`           | The mount path                                                                                                                                        |
| `server`, `port`  | The record's `server.hostname` and `server.port`                                                                                                      |
| `server-instance` | The record's `server.instance`                                                                                                                        |
| `ip`, `agent`     | The record's `ip` and `agent`: the source client's address and `User-Agent` (Icecast-KH; empty for a Liquidsoap source), on connect and on disconnect |

Every field is sent under every `compatibility` value, and each notification
uses its own URL.

## 8. Lifecycle

- **`source_timeout`.** A source client that sends no data for this long is
  dropped as if it had disconnected. It is read when a source client connects.
- **Disconnect.** When a feeder goes away, the mount keeps its listeners while
  a fallback has something to send ([mounts.md §6](mounts.md#6-fallback)) or
  while `linger` lasts ([mounts.md §1.1](mounts.md#11-linger)); otherwise the
  mount ends ([mounts.md §1](mounts.md#1-lifecycle)).
- **Reconnect.** A source client that logs in again while the old connection
  is still open is refused as taken. The old connection's late disconnect MUST
  NOT take the mount from the new one. A source client that comes back with a
  stream of another format is accepted ([mounts.md §3](mounts.md#3-the-format-match-rule)).
- **Connect and disconnect callbacks.** The mount's `on_connect` runs when a
  feeder takes the mount, with the record of the source-connect event
  ([observability.md §1](observability.md#1-events)). Its `on_disconnect`
  runs when the feeder leaves, with the record of the source-disconnect event,
  and is the one the mount had
  when the feeder connected: a reload between the two does not change it.
  They run in the background; a failure is logged and never stops the stream.
  Icecast's `<on-connect>` and `<on-disconnect>` commands are callbacks that
  run a command ([icecast-config.md](icecast-config.md)).
- **Events.** The server raises a source-connect and a source-disconnect event
  for every feeder, a Liquidsoap source included. Their fields and ordering
  are in [observability.md](observability.md).
- **Log lines.** Login, exit and refusals are written to the error log with
  their reason ([observability.md](observability.md)).

## 9. Compatibility

| Rule                                                       | Values                                                                                                                                                                           |
| ---------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| refusal statuses ([§4](#4-replies))                        | mount taken, `max_sources`, unparsable stream: `icecast-2.5` `409`, `503`, `415`; the others `403`. Bad credentials, no `Content-Type` and the reserved path do not depend on it |
| unknown mount on a mount command ([§6](#6-admin-requests)) | `icecast-2.5` `404`; the others `400`                                                                                                                                            |

Every other rule holds under every value.

## 10. Conformance

**Binding**

- PUT, SOURCE and Shoutcast source clients feed a mount; the order of checks
  (reserved, credentials, taken, `max_sources`); the status of each refusal per
  `compatibility`; `WWW-Authenticate` on `401`.
- No credentials needed when the password is empty; per-mount credentials
  override global; `source_decision` replaces only the credentials check, is
  not called for a reserved path, and its refusal is a `401`.
- Every reserved path of [§2](#2-reserved-paths) is refused, whether its page
  is on or off.
- The source decision request carries `agent`, `headers`, `server` and
  `compatibility`; the source events carry `ip`, `agent` and `server`, the
  disconnect one included, and `mount_remove` sends them.
- `stream_auth` request fields and its admit/refuse rule; `mount_add` and
  `mount_remove` fire once each, to their own URL, with the fields above.
- Identity headers reach listeners under the `icy-*` names and a header with a
  line break does not; the packets of a stream are relayed byte-identical. A
  listener joining after the source client reconnected with a new `ice-name`
  gets the new `icy-name`.
- Admin requests: a mount command is served with the source's credentials, or
  the admin's when `admin_password` is set, whether or not it is set; an admin
  page answers `401` with the challenge without `admin_password`, and a source
  client's credentials never open one; the errors of
  [§6](#6-admin-requests), the metadata statuses and the `song`/`title` rule,
  `killsource` (and `manage` `kick`) ends the source client like a disconnect.
- A source client is never refused for its format; the `mount_add` and
  `mount_remove` notifications and the mount's own command callbacks all run
  when both exist.
- The default source password `hackme` logs a warning at startup and at each
  reload while a mount still uses it.
- A source client silent for `source_timeout` is dropped; a late disconnect of
  a replaced source client leaves the new one.
- The mount's connect and disconnect callbacks run once each, the disconnect
  with the value read at connect; the source events fire once for a source
  client and once for a Liquidsoap source.

**Incidental**: reply bodies and wording, the exact `source_timeout`
tolerance, callback start latency, log wording.

**Doubles**: a scripted source client per protocol, a fake backend answering
`stream_auth` and the notifications, a clock for `source_timeout`.

Establish that tests ran: count the source clients admitted and refused and the
callbacks run, and fail when zero.
